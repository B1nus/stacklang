import sys
from parser import *

def evaluate(statements, labels, stack_size=10):
    stack = [0] * stack_size
    statement_pointer = 0
    opcode = None
    while opcode != "HALT":
        statement = statements[statement_pointer]
        opcode = statement[0]

        match opcode:
            case "PUSH":
                stack.append(statement[1])
            case "POP":
                stack.pop()
            case "SUB":
                stack.append(-stack.pop() + stack.pop())
            case "ADD":
                stack.append(stack.pop() + stack.pop())
            case "READ":
                stack.append(int(input("READ: ")))
            case "PRINT":
                print(statement[1])
            case "JUMP.EQ.0":
                if stack[-1] == 0:
                    statement_pointer = labels[statement[1]]
                    continue
            case "JUMP.GT.0":
                if stack[-1] > 0:
                    statement_pointer = labels[statement[1]]
                    continue
            case "JUMP":
                statement_pointer = labels[statement[1]]
                continue

        statement_pointer += 1


with open(sys.argv[1]) as file:
    tokens = tokenize(file.read())
    statements, labels = parse(tokens)
    evaluate(statements, labels)
