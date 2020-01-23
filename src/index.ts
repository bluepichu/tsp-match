import * as ts from "typescript";

export default (registrationContext: ts.PreprocessorRegistrationContext) => {
	console.log("Registering tsp-match");
	registrationContext.registerPreprocessorStatement("match", matchStatement);
};

export function matchStatement(args: ts.NodeArray<ts.Statement>, range: ts.TextRange, context: ts.PreprocessorContext) {
	// assume that args are right, still need to figure out emitting diagnostics on the whole thing
	let ret = substituteSwitchStatement(args[0] as ts.SwitchStatement, context);
	ret.pos = args[0].pos;
	ret.end = args[0].end;
	cleanup(ret, range);
	return ret;
}

interface CaseGroup {
	cases: MatchResultWithNode[];
	statements: ts.NodeArray<ts.Statement>;
}

interface MatchResultWithNode extends MatchResult {
	node: ts.Node;
	index: number;
}

interface MatchResult {
	match: ts.Expression;
	bind: [string, ts.Expression][];
}

function substituteSwitchStatement(node: ts.SwitchStatement, context: ts.PreprocessorContext): ts.Statement {
	let exp = node.expression;
	let argTemp = ts.createIdentifier("_a"); // ts.createTempVariable(undefined);

	let argDecl = ts.createVariableDeclaration(ts.getMutableClone(argTemp), undefined, exp);
	let argCompute = ts.createVariableStatement(
		undefined, // modifiers
		[argDecl]
	);

	let groups: CaseGroup[] = [];
	let currentGroup: CaseGroup | undefined;
	let caseIndex = 0;

	for (let clause of node.caseBlock.clauses) {
		if (currentGroup === undefined) {
			currentGroup = { cases: [], statements: ts.createNodeArray() };
		}

		switch (clause.kind) {
			case ts.SyntaxKind.DefaultClause: {
				let tru = ts.createTrue();
				// FIXME: bad choice of node here?
				currentGroup.cases.push({ node: clause, match: tru, bind: [], index: caseIndex });
				caseIndex++;
				break;
			}

			case ts.SyntaxKind.CaseClause: {
				let matchCase = match(ts.getMutableClone(argTemp), clause.expression, context)
				currentGroup.cases.push({
					...matchCase,
					node: clause.expression,
					index: caseIndex
				});
				caseIndex++;
				break;
			}

			default: {
				unreachable(clause);
			}
		}

		if (clause.statements.length > 0) {
			// Assumption: all blocks end with break
			// We should probably enforce this!
			currentGroup.statements = clause.statements;
			groups.push(currentGroup);
			currentGroup = undefined;
		}
	}

	if (currentGroup !== undefined) {
		// All cases should lead to code
		throw new Error();
	}

	if (groups.length === 0) {
		// There should be at least one case
		throw new Error();
	}

	let allVars = new Map<string, ts.Identifier>();

	for (let group of groups) {
		for (let cs of group.cases) {
			for (let bnd of cs.bind) {
				if (!allVars.has(bnd[0])) {
					allVars.set(bnd[0], ts.createIdentifier(bnd[0]));
				}
			}
		}
	}

	let varDecl = ts.createVariableStatement(
		undefined,
		ts.createVariableDeclarationList(
			[...allVars.values()].map((vr) => ts.createVariableDeclaration(vr))
		)
	);

	let options = groups.map(({ cases, statements }) => ({
		condition:
			cases
				.map((cs) => ts.createLogicalAnd(
					cs.match,
					cs.bind.reduceRight<ts.Expression>((prev, next) => ts.createComma(ts.createParen(ts.createAssignment(ts.createIdentifier(next[0]), next[1])), prev), ts.createTrue())
				))
				.reduce((a, b) => ts.createLogicalOr(a, b)),
		statements
		// statements: [
		// 	ts.createVariableStatement(
		// 		undefined,
		// 		[...varList.keys()]
		// 			.map((v) => {
		// 				let ret = ts.createVariableDeclaration(
		// 					ts.createIdentifier(v),
		// 					undefined,
		// 					cases.reduceRight<ts.Expression>((prev, next) => {
		// 						if (next.bind.has(v)) {
		// 							return ts.createConditional(
		// 								ts.createStrictEquality(
		// 									ts.getMutableClone(caseTemp),
		// 									ts.createNumericLiteral(next.index.toString())
		// 								),
		// 								next.bind.get(v)!,
		// 								prev
		// 							);
		// 						} else {
		// 							return prev;
		// 						}
		// 					}, ts.createVoidZero())
		// 				);

		// 				ret.flags = ts.NodeFlags.Let;
		// 				return ret;
		// 			})
		// 	) as ts.Statement
		// ].concat(statements)
	}));

	let conditional = options.reduceRight<ts.Statement | undefined>((prev, next) => {
		let block = ts.createBlock(next.statements);
		let result = ts.createIf(next.condition, block, prev);
		return result;
	}, undefined)!;

	let stmts = ts.createNodeArray([argCompute, varDecl, conditional]);
	let block = ts.createBlock(stmts);
	return block;
}

function match(path: ts.Expression, matcher: ts.Expression, context: ts.PreprocessorContext): MatchResult {
	switch (matcher.kind) {
		case ts.SyntaxKind.Identifier: {
			// assumption: this is actually a binding pattern
			// let assn = ts.createAssignment(matcher, path);
			// return {
			// 	match: ts.createTrue(),
			// 	bind: [ assn ]
			// };

			// FIXME: due to the requirement in the language server that the AST be well-ordered, we can't do this!
			// Figure out what to do here...
			context.emitDiagnostic({
				code: 99998,
				category: ts.DiagnosticCategory.Error,
				messageText: "Renaming destructures are not allowed.",
				start: matcher.pos,
				length: matcher.end - matcher.pos
			});

			return {
				match: ts.createFalse(),
				bind: []
			};
		}

		case ts.SyntaxKind.ObjectLiteralExpression: {
			let tests: MatchResult[] = [{
				match: path,
				bind: []
			}];

			for (let element of (matcher as ts.ObjectLiteralExpression).properties) {
				switch (element.kind) {
					case ts.SyntaxKind.MethodDeclaration:
					case ts.SyntaxKind.GetAccessor:
					case ts.SyntaxKind.SetAccessor: {
						// illegal in this position
						throw new Error();
					}

					case ts.SyntaxKind.ShorthandPropertyAssignment: {
						// this is actually a binding pattern
						let access = ts.createPropertyAccess(path, element.name);
						tests.push({
							match: ts.createTrue(),
							bind: [[element.name.escapedText as string, access]]
						});
						break;
					}

					case ts.SyntaxKind.SpreadAssignment: {
						// TODO: how do we want to handle this?  should it be allowed?
						throw new Error();
					}

					case ts.SyntaxKind.PropertyAssignment: {
						// otherwise, we should do a comparison
						let fieldName;

						switch (element.name.kind) {
							case ts.SyntaxKind.Identifier: {
								fieldName = element.name.escapedText as string;
								break;
							}

							case ts.SyntaxKind.StringLiteral:
							case ts.SyntaxKind.NumericLiteral: {
								fieldName = element.name.text;
								break;
							}

							case ts.SyntaxKind.ComputedPropertyName: {
								// ...no.
								throw new Error();
							}

							default: {
								return unreachable(element.name);
							}
						}

						let fieldIdent = ts.createIdentifier(fieldName);
						let childPath = ts.createPropertyAccess(ts.getMutableClone(path), fieldIdent);
						tests.push(match(childPath, element.initializer, context));
						break;
					}

					default: {
						return unreachable(element);
					}
				}
			}

			return tests.reduce<MatchResult>((prev, next) => {
				let and = ts.createLogicalAnd(prev.match, next.match);

				return {
					match: and,
					bind: prev.bind.concat(next.bind)
				};
			}, {
				match: ts.createTrue(),
				bind: []
			});
		}

		default: {
			// Otherwise, we require an exact match
			let condition = ts.createStrictEquality(path, matcher);
			// Range will be inherited from children
			return {
				match: condition,
				bind: []
			};
		}
	}
}

function unreachable(arg: never): never {
	throw new Error("unreachable?");
}

function cleanup(node: ts.Node, range: ts.TextRange) {
	let curPos = node.pos;
	visit(node);

	function visit(node: ts.Node) {
		let shouldMark = false;
		if ((node.flags & ts.NodeFlags.Synthesized) && !(node.flags & ts.NodeFlags.CreatedInPreprocessor)) {
			shouldMark = true;
		}

		let pos = node.pos === -1 ? curPos : node.pos;
		curPos = pos;
		ts.forEachChild(node, visit);
		let end = node.end === -1 ? curPos : node.end;
		curPos = end;

		if (shouldMark) {
			node.flags |= ts.NodeFlags.CreatedInPreprocessor;
			(node as ts.PreprocessedNode).preprocessor = {
				tag: range,
				self: { pos, end }
			};
		}
	}
}