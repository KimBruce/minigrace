  method visitMatchCase (node: share.MatchCase) → Boolean {
        // expression being matched and its type
        def matchee = node.value
        var matcheeType: ObjectType := typeOf(matchee)
        //Note: currently only one matchee is supported

        // Keep track of parameter types in case as well as return types
        def paramTypesList: List⟦ObjectType⟧ = emptyList⟦ObjectType⟧
        def returnTypesList: List⟦ObjectType⟧ = emptyList⟦ObjectType⟧

        //goes through each case and accumulates its parameter and return types
        for(node.cases) do{block →

            if(block.isMatchingBlock.not) then{
              DialectError.raise("1518: The case you are matching to, " ++
                "{stripNewLines(block.toGrace(0))} on line {block.line}, " ++
                "has more than one argument on the left side. This is not " ++
                "currently allowed.") with (matchee)
            }

            //If param is a general case(ie. n:Number), accumulate its type to
            //paramTypesList; ignore if it is a specific case(ie. 47)
            def blockParam : AstNode = block.params.at(1)
            if (debug) then {
                io.error.write"\nMy dtype is {blockParam.dtype}"
            }
            if (("string" ≠ blockParam.dtype.kind)
                                  && {"num" ≠ blockParam.dtype.kind}) then {
                def typeOfParam = anObjectType.fromDType(blockParam.dtype)

                if (paramTypesList.contains(typeOfParam).not) then {
                    paramTypesList.add(typeOfParam)
                }
            }

            //Build return type collection
            def blockReturnType : ObjectType = objectTypeFromBlock(block)
            if (returnTypesList.contains(blockReturnType).not) then {
              returnTypesList.add(blockReturnType)
            }
        }
        // Type covered by parameters in case (types are variants)
        def paramType: ObjectType = ot.fromObjectTypeList(paramTypesList)

        // Type returned by variant of all return types in cases
        def returnType: ObjectType = ot.fromObjectTypeList(returnTypesList)

        if (debug) then {
            io.error.write "\nparamType now equals: {paramType}"
            io.error.write "\nreturnType now equals: {returnType}"
        }


        // If matchee not covered by cases then raise a type error
        if (matcheeType.isSubtypeOf(paramType).not) then {
            DialectError.raise("1519: the matchee " ++
                "`{stripNewLines(matchee.toGrace(0))}` of type {matcheeType} "++
                "on line {matchee.line} does not match the type(s) " ++
                "{paramTypesList} of the case(s)") with (matchee)
        }

        // returnType is type of the match-case statement
        cache.at(node) put (returnType)

        false
    }