from parser import *
from pathlib import Path
import shutil
import subprocess
import sys
import os

TEXT_CONST = """\ttext{id} db "{text}",10
\tlen{id} equ $ - text{id}"""
PRINT_PROGRAM = """\tmov rax, 1
\tmov rdi, 1
\tmov rsi, text{id}
\tmov rdx, len{id}
\tsyscall"""
READ_PROGRAM = """\tmov rax, 0
\tmov rdi, 0
\tmov rsi, read
\tmov rdx, 16
\tsyscall
\tcall _conversion_loop
\tpush rbx"""
CONVERSION_LOOP = """_conversion_loop:
\tmov rbx, 0
\tmov rdi, read

_conversion_loop_inner:
\tmovzx rcx, byte [rdi]
\tcmp rcx, 10
\tje _conversion_done
\tsub rcx, '0'
\tmov rax, rbx
\tmov rdx, 10
\timul rdx
\tmov rbx, rax
\tadd rbx, rcx
\tinc rdi
\tjmp _conversion_loop_inner

_conversion_done:
\tret"""
SUB_PROGRAM = """\tpop rax
\tpop rbx
\tsub rbx, rax
\tpush rbx"""
ADD_PROGRAM = """\tpop rax
\tpop rbx
\tadd rbx, rax
\tpush rbx"""
SYSEXIT = """\tmov rax, 60
\tmov rdi, 0
\tsyscall
"""
START = """
section .text
\tglobal _start

_start:"""
TEXT_SECTION = """section .data
\tnumber dq 0
\ttext0 db "READ: "
\tlen0 equ $ - text0"""

def compile(statements, labels, outpath):
    section_text = [TEXT_SECTION]
    section_bss = ["section .bss\n\tread resb 16"]
    _start = [START]
    text_counter = 1
    for statement_id, statement in enumerate(statements):
        _start.append("; -- {} --".format(statement[0]))
        match statement[0]:
            case "PRINT":
                section_text.append(TEXT_CONST.format(id=str(text_counter), text=statement[1]))
                _start.append(PRINT_PROGRAM.format(id=str(text_counter)))
                text_counter += 1
            case "READ":
                _start.append(PRINT_PROGRAM.format(id="0"))
                _start.append(READ_PROGRAM)
            case "PUSH":
                _start.append("\tpush {}".format(statement[1]))
            case "POP":
                _start.append("\tpop")
            case "SUB":
                _start.append(SUB_PROGRAM)
            case "ADD":
                _start.append(ADD_PROGRAM)
            case "HALT":
                _start.append(SYSEXIT)
            case "JUMP.EQ.0":
                _start.append("\tcmp qword [rsp], 0\n\tje {}".format(statement[1]))
            case "JUMP.GT.0":
                _start.append("\tcmp qword [rsp], 0\n\tjg {}".format(statement[1]))
            case "JUMP":
                _start.append("\tjmp {}".format(statement[1]))
            case "LABEL":
                _start.append("{}:".format(statement[1]))


    assembly_array = section_text + [""] + section_bss + [""] + _start + [CONVERSION_LOOP]
    assembly = "\n".join(assembly_array)

    with open(outpath, "w") as outfile:
        outfile.write(assembly)

    stem = Path(outpath).stem
    if not shutil.which("nasm"):
        exit("The compiler depends on nasm. Please install nasm in order to compile. Or assemble the " + stem + ".asm file yourself.")
    subprocess.run(["nasm", "-f", "elf64", "-g", "-F", "dwarf", outpath, "-o", stem + ".o"])
    subprocess.run(["ld", stem + ".o", "-o", stem])
    # subprocess.run(["rm", stem + ".o"])
    subprocess.run(["./" + stem])

source_path = sys.argv[1]
with open(source_path) as file:
    tokens = tokenize(file.read())
    statements, labels = parse(tokens)
    if len(sys.argv) >= 3:
        compile(statements, labels, sys.argv[2])
    else:
        compile(statements, labels, Path(source_path).stem + ".asm")
