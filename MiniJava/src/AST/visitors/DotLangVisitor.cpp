#include <AST/visitors/DotLangVisitor.h>

using namespace AstTree;

void CDotLangVisitor::addEdge( const std::string& nodeFromName, const std::string& nodeToName ) {
    treeEdges[nodeFromName].push_back( nodeToName );
}

std::string CDotLangVisitor::GetTraversalInDotLanguage() const {
    std::stringstream sstream;
    sstream << "digraph {" << std::endl;
    sstream << '\t' << "ordering = out;" << std::endl;
    for ( auto it = treeEdges.begin(); it != treeEdges.end(); ++it ) {
        const std::string& fromNode = it->first;
        for ( const std::string& toNode : it->second ) {
            sstream << '\t' << fromNode << " -> " << toNode << ';' << std::endl;
        }
    }
    sstream << '}' << std::endl;

    return sstream.str();
}

void CDotLangVisitor::Clear() {
    treeEdges.clear();
    nodeTypeLastUsedIndex.clear();
    visitedNodeStack.clear();
}

/*__________ Access Modifiers __________*/

void CDotLangVisitor::Visit( const CPublicAccessModifier* modifier ) {
    std::string nodeName = generateNodeName( CAstNodeNames::ACCESS_MOD_PUBLIC );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CPrivateAccessModifier* modifier ) {
    std::string nodeName = generateNodeName( CAstNodeNames::ACCESS_MOD_PRIVATE );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    onNodeExit( nodeName );
}

/*__________ Expressions __________*/

void CDotLangVisitor::Visit( const CBinaryExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_BINARY );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    expression->LeftOperand()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    addEdge( nodeName,  generateNodeName( operatorName( expression->Operation() ) ) );

    expression->RightOperand()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CBracketExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_BRACKET );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    expression->ContainerExpression()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    expression->IndexExpression()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CNumberExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_NUMBER );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    std::string valueNodeName = "\"" + generateNodeName( "Value" ) + ": ";
    valueNodeName += std::to_string( expression->Value() ) + "\"";
    addEdge( nodeName, valueNodeName );

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CLogicExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_LOGIC );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    std::string valueNodeName = "\"" + generateNodeName( "Value" ) + ": ";
    valueNodeName += std::string( expression->Value() ? "true" : "false" ) + '\"';
    addEdge( nodeName, valueNodeName );

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CIdExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_ID );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    std::string valueNodeName = "\"" + generateNodeName( "Id" ) + ": " + expression->Name() + "\"";
    addEdge( nodeName, valueNodeName );

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CLengthExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_LENGTH );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    expression->LengthTarget()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CMethodExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_METHOD );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    expression->CallerExpression()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    expression->MethodId()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    expression->Arguments()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CThisExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_THIS );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CNewArrayExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_NEW_ARRAY );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    expression->LengthExpression()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CNewIdExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_NEW_ID );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    expression->TargetId()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CNegateExpression* expression ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_NEGATE );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    expression->TargetExpression()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

/*__________ Statements __________*/

void CDotLangVisitor::Visit( const CAssignIdStatement* statement ) {
    std::string nodeName = generateNodeName( CAstNodeNames::STAT_ASSIGN_ID );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    statement->LeftPart()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    statement->RightPart()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CAssignIdWithIndexStatement* statement ) {
    std::string nodeName = generateNodeName( CAstNodeNames::STAT_ASSIGN_ID_WITH_INDEX );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    statement->LeftPartId()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    statement->LeftPartIndex()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    statement->RightPart()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CPrintStatement* statement ) {
    std::string nodeName = generateNodeName( CAstNodeNames::STAT_PRINT );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    statement->PrintTarget()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CConditionalStatement* statement ) {
    std::string nodeName = generateNodeName( CAstNodeNames::STAT_CONDITIONAL );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    statement->Condition()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    statement->PositiveTarget()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    statement->NegativeTarget()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CWhileLoopStatement* statement ) {
    std::string nodeName = generateNodeName( CAstNodeNames::STAT_WHILE_LOOP );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    statement->Condition()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    statement->Body()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CBracesStatement* statement ) {
    std::string nodeName = generateNodeName( CAstNodeNames::STAT_BRACES );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    statement->List()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

/*__________ Type Modifiers __________*/

void CDotLangVisitor::Visit( const CIntTypeModifier* typeModifier ) {
    std::string nodeName = generateNodeName( CAstNodeNames::TYPE_MOD_INT );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CBooleanTypeModifier* typeModifier ) {
    std::string nodeName = generateNodeName( CAstNodeNames::TYPE_MOD_BOOL );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CIntArrayTypeModifier* typeModifier ) {
    std::string nodeName = generateNodeName( CAstNodeNames::TYPE_MOD_INT_ARRAY );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CIdTypeModifier* typeModifier ) {
    std::string nodeName = generateNodeName( CAstNodeNames::TYPE_MOD_ID );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    typeModifier->TypeId()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

/*__________ Other (except lists) __________*/

void CDotLangVisitor::Visit( const CVarDeclaration* declaration ) {
    std::string nodeName = generateNodeName( CAstNodeNames::VAR_DECL );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    declaration->Type()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    declaration->Id()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CMethodArgument* argument ) {
    std::string nodeName = generateNodeName( CAstNodeNames::METH_ARG );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    argument->Type()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    argument->Id()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CMethodDeclaration* declaration ) {
    std::string nodeName = generateNodeName( CAstNodeNames::METH_DECL );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    declaration->AccessModifier()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    declaration->TypeModifier()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    declaration->MethodId()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    declaration->MethodArguments()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    declaration->VarDeclarations()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    declaration->Statements()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    declaration->ReturnExpression()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CMainClass* mainClass ) {
    std::string nodeName = generateNodeName( CAstNodeNames::MAIN_CLASS );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    mainClass->ClassName()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    mainClass->ClassArgsName()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    mainClass->Statements()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CClassDeclaration* declaration ) {
    std::string nodeName = generateNodeName( CAstNodeNames::CLASS_DECL );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    declaration->ClassName()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    declaration->VarDeclarations()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    declaration->MethodDeclarations()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    if ( declaration->HasParent() ) {
        declaration->ExtendsClassName()->Accept( this );

        addEdge( nodeName, visitedNodeStack.back() );
        visitedNodeStack.pop_back();
    }

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CProgram* program ) {
    std::string nodeName = generateNodeName( CAstNodeNames::PROGRAM );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    program->MainClass()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    program->ClassDeclarations()->Accept( this );
    addEdge( nodeName, visitedNodeStack.back() );
    visitedNodeStack.pop_back();

    onNodeExit( nodeName );
}

/*__________  Lists __________*/

void CDotLangVisitor::Visit( const CExpressionList* list ) {
    std::string nodeName = generateNodeName( CAstNodeNames::EXP_LIST );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    const std::vector< std::unique_ptr<const CExpression> >& expressions = list->Expressions();
    for ( auto it = expressions.begin(); it != expressions.end(); ++it ) {
        ( *it )->Accept( this );

        addEdge( nodeName, visitedNodeStack.back() );
        visitedNodeStack.pop_back();
    }

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CStatementList* list ) {
    std::string nodeName = generateNodeName( CAstNodeNames::STAT_LIST );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    const std::vector< std::unique_ptr<const CStatement> >& statements = list->Statements();
    // must be reversed before being used
    for ( auto rit = statements.rbegin(); rit != statements.rend(); ++rit ) {
        ( *rit )->Accept( this );

        addEdge( nodeName, visitedNodeStack.back() );
        visitedNodeStack.pop_back();
    }

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CVarDeclarationList* list ) {
    std::string nodeName = generateNodeName( CAstNodeNames::VAR_DECL_LIST );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    const std::vector< std::unique_ptr<const CVarDeclaration> >& varDeclarations = list->VarDeclarations();
    for ( auto it = varDeclarations.begin(); it != varDeclarations.end(); ++it ) {
        ( *it )->Accept( this );

        addEdge( nodeName, visitedNodeStack.back() );
        visitedNodeStack.pop_back();
    }

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CMethodArgumentList* list ) {
    std::string nodeName = generateNodeName( CAstNodeNames::METH_ARG_LIST );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    const std::vector< std::unique_ptr<const CMethodArgument> >& methodArguments = list->MethodArguments();
    for ( auto it = methodArguments.begin(); it != methodArguments.end(); ++it ) {
        ( *it )->Accept( this );

        addEdge( nodeName, visitedNodeStack.back() );
        visitedNodeStack.pop_back();
    }

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CMethodDeclarationList* list ) {
    std::string nodeName = generateNodeName( CAstNodeNames::METH_DECL_LIST );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    const std::vector< std::unique_ptr<const CMethodDeclaration> >& methodDeclarations = list->MethodDeclarations();
    for ( auto it = methodDeclarations.begin(); it != methodDeclarations.end(); ++it ) {
        ( *it )->Accept( this );

        addEdge( nodeName, visitedNodeStack.back() );
        visitedNodeStack.pop_back();
    }

    onNodeExit( nodeName );
}

void CDotLangVisitor::Visit( const CClassDeclarationList* list ) {
    std::string nodeName = generateNodeName( CAstNodeNames::CLASS_DECL_LIST );
    onNodeEnter( nodeName );
    visitedNodeStack.push_back( nodeName );

    const std::vector< std::unique_ptr<const CClassDeclaration> >& classDeclarations = list->ClassDeclarations();
    for ( auto it = classDeclarations.begin(); it != classDeclarations.end(); ++it ) {
        ( *it )->Accept( this );

        addEdge( nodeName, visitedNodeStack.back() );
        visitedNodeStack.pop_back();
    }

    onNodeExit( nodeName );
}
