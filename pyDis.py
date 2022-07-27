registers = [ 
    {'name':'rax','size':64,'code':0b000},
    {'name':'rbx','size':64,'code':0b011},
    {'name':'rcx','size':64,'code':0b001},
    {'name':'rdx','size':64,'code':0b010},
    {'name':'rsp','size':64,'code':0b100},
    {'name':'rsi','size':64,'code':0b110},
    {'name':'rdi','size':64,'code':0b111},
    {'name':'rbp','size':64,'code':0b101},
    {'name':'r8','size':64,'code':0b1000},
    {'name':'r9','size':64,'code':0b1001},
    {'name':'r10','size':64,'code':0b1010},
    {'name':'r11','size':64,'code':0b1011},
    {'name':'r12','size':64,'code':0b1100},
    {'name':'r13','size':64,'code':0b1101},
    {'name':'r14','size':64,'code':0b1110},
    {'name':'r15','size':64,'code':0b1111},

    {'name':'eax','size':32,'code':0b000},
    {'name':'ebx','size':32,'code':0b011},
    {'name':'ecx','size':32,'code':0b001},
    {'name':'edx','size':32,'code':0b010},
    {'name':'esp','size':32,'code':0b100},
    {'name':'esi','size':32,'code':0b110},
    {'name':'edi','size':32,'code':0b111},
    {'name':'ebp','size':32,'code':0b101},
    {'name':'r8d','size':32,'code':0b1000},
    {'name':'r9d','size':32,'code':0b1001},
    {'name':'r10d','size':32,'code':0b1010},
    {'name':'r11d','size':32,'code':0b1011},
    {'name':'r12d','size':32,'code':0b1100},
    {'name':'r13d','size':32,'code':0b1101},
    {'name':'r14d','size':32,'code':0b1110},
    {'name':'r15d','size':32,'code':0b1111},

    {'name':'ax','size':16,'code':0b000},
    {'name':'bx','size':16,'code':0b011},
    {'name':'cx','size':16,'code':0b001},
    {'name':'dx','size':16,'code':0b010},
    {'name':'sp','size':16,'code':0b100},
    {'name':'si','size':16,'code':0b110},
    {'name':'di','size':16,'code':0b111},
    {'name':'bp','size':16,'code':0b101},
    {'name':'r8w','size':16,'code':0b1000},
    {'name':'r9w','size':16,'code':0b1001},
    {'name':'r10w','size':16,'code':0b1010},
    {'name':'r11w','size':16,'code':0b1011},
    {'name':'r12w','size':16,'code':0b1100},
    {'name':'r13w','size':16,'code':0b1101},
    {'name':'r14w','size':16,'code':0b1110},
    {'name':'r15w','size':16,'code':0b1111},

    {'name':'al','size':8,'code':0b000},
    {'name':'bl','size':8,'code':0b011},
    {'name':'cl','size':8,'code':0b001},
    {'name':'dl','size':8,'code':0b010},
    {'name':'ah','size':8,'code':0b100},
    {'name':'bh','size':8,'code':0b111},
    {'name':'ch','size':8,'code':0b101},
    {'name':'dh','size':8,'code':0b110},
    {'name':'r8b','size':8,'code':0b1000},
    {'name':'r9b','size':8,'code':0b1001},
    {'name':'r10b','size':8,'code':0b1010},
    {'name':'r11b','size':8,'code':0b1011},
    {'name':'r12b','size':8,'code':0b1100},
    {'name':'r13b','size':8,'code':0b1101},
    {'name':'r14b','size':8,'code':0b1110},
    {'name':'r15b','size':8,'code':0b1111},
]

operators = [
            {'name':'mov', 'operands':2,'opcode': 0b100010,        'iopcode':0b110001000},
            {'name':'add', 'operands':2,'opcode': 0b000000,        'iopcode':0b100000000},
            {'name':'adc', 'operands':2,'opcode': 0b000100,        'iopcode':0b100000010},
            {'name':'sub', 'operands':2,'opcode': 0b001010,        'iopcode':0b100000101},
            {'name':'sbb', 'operands':2,'opcode': 0b000110,        'iopcode':0b100000011},
            {'name':'or',  'operands':2,'opcode': 0b000010,        'iopcode':0b100000001},
            {'name':'xor', 'operands':2,'opcode': 0b001100,        'iopcode':0b100000110},
            {'name':'and', 'operands':2,'opcode': 0b001000,        'iopcode':0b100000100},
            {'name':'cmp', 'operands':2,'opcode': 0b001110,        'iopcode':0b100000111},
            {'name':'shl', 'operands':2,'opcode': 0b0,             'iopcode':0b110000100},
            {'name':'shr', 'operands':2,'opcode': 0b0,             'iopcode':0b110000101},
            {'name':'test','operands':2,'opcode': 0b0000100001,    'iopcode':0b111101000},
            {'name':'xchg','operands':2,'opcode': 0b0000100001,    'iopcode':0b0},
            
            # {'name':'xadd','operands':2,'opcode': 0b00001111110000,'iopcode':0b0},
            {'name':'bsf', 'operands':2,'opcode': 0b00001111101111,'iopcode':0b0},
            {'name':'bsr', 'operands':2,'opcode': 0b00001111101111,'iopcode':0b0},
            {'name':'imul','operands':2,'opcode': 0b00001111101011,'iopcode':0b0},
           
            {'name':'Amov', 'operands':2,'opcode':  0b1011,},  # wreg
            {'name':'Apush', 'operands':1,'opcode': 0b01011,}, #reg
            {'name':'Apop', 'operands':1,'opcode':  0b01010,}, #reg
            {'name':'Ainc', 'operands':1,'opcode':  0b01000,}, #reg
            

            {'name':'inc', 'operands':1,'opcode':0b111111,  'rcode':0b000},
            {'name':'dec', 'operands':1,'opcode':0b111111,  'rcode':0b001},
            {'name':'call','operands':1,'opcode':0b111111,  'rcode':0b010},
            {'name':'jmp', 'operands':1,'opcode':0b111111,  'rcode':0b100},
            {'name':'push','operands':1,'opcode':0b111111,  'rcode':0b110},
            
            {'name':'not', 'operands':1,'opcode':0b111101,  'rcode':0b010},
            {'name':'neg', 'operands':1,'opcode':0b111101,  'rcode':0b011},
            {'name':'imul','operands':1,'opcode':0b111101,  'rcode':0b101},
            {'name':'idiv','operands':1,'opcode':0b111101,  'rcode':0b111},
            
            {'name':'pop', 'operands':1,'opcode':0b100011,  'rcode':0b000},
            {'name':'shl', 'operands':1,'opcode':0b110100,  'rcode':0b100},
            {'name':'shr',' operands':1,'opcode':0b110100,  'rcode':0b101},

            {'name':'ret', 'operands':1,'opcode':0b11000010},
            


            {'name':'ret','operands':0,'opcode':0b11000011},
            {'name':'std','operands':0,'opcode':0b11111101},
            {'name':'stc','operands':0,'opcode':0b11111001},
            {'name':'clc','operands':0,'opcode':0b11111000},
            {'name':'cld','operands':0,'opcode':0b11111100},
            {'name':'syscall','operands':0,'opcode':0b0000111100000101},
            {'name':'jo','operands': 1,'opcode':0b0000},
            {'name':'jno','operands': 1,'opcode':0b0001},
            {'name':'jb','operands': 1,'opcode':0b0010},
            {'name':'jnae','operands': 1,'opcode':0b0010},
            {'name':'jnb','operands': 1,'opcode':0b0011},
            {'name':'jae','operands': 1,'opcode':0b0011},
            {'name':'je','operands': 1,'opcode':0b0100},
            {'name':'jz','operands': 1,'opcode':0b0100},
            {'name':'jne','operands': 1,'opcode':0b0101},
            {'name':'jnz','operands': 1,'opcode':0b0101},
            {'name':'jbe','operands': 1,'opcode':0b0110},
            {'name':'jna','operands': 1,'opcode':0b0110},
            {'name':'jnbe','operands': 1,'opcode':0b0111},
            {'name':'ja','operands': 1,'opcode':0b0111},
            {'name':'js','operands': 1,'opcode':0b1000},
            {'name':'jns','operands': 1,'opcode':0b1001},
            {'name':'jp','operands': 1,'opcode':0b1010},
            {'name':'jpe','operands': 1,'opcode':0b1010},
            {'name':'jnp','operands': 1,'opcode':0b1011},
            {'name':'jpo','operands': 1,'opcode':0b1011},
            {'name':'jl','operands': 1,'opcode':0b1100},
            {'name':'jnge','operands': 1,'opcode':0b1100},
            {'name':'jnl','operands': 1,'opcode':0b1101},
            {'name':'jge','operands': 1,'opcode':0b1101},
            {'name':'jle','operands': 1,'opcode':0b1110},
            {'name':'jng','operands': 1,'opcode':0b1110},
            {'name':'jnle','operands': 1,'opcode':0b1111},
            {'name':'jg','operands': 1,'opcode':0b1111},

        ]

# dictt = {'f':{1:{},
#               2:{}
              
#               },

#         }




memory_size = {8:'BYTE',16:'WORD',32:'DWORD',64:'QWORD'}



class Instructions:
    def __init__(self):
        self.operator = {}
        self.operands = []
        self.operands_len = 0


        self.prefix = 0

        self.rex = False
        self.rex_w = 0
        self.rex_r = 0
        self.rex_x = 0
        self.rex_b = 0

        self.opcode = 0
        self.opcode_size = 0
        self.d = 0
        self.w = 0

        self.mod = 0
        self.reg = 0
        self.rm = 0

        self.memory = False

        self.sib = False
        self.base = {}
        self.index = {}
        self.scale = 0
        self.disp = 0
        self.disp_size = 0

        self.imd = False
        self.imd_size = 0

        self.opr_size = 0

        self.addr_size = 0

        self.addrPrefix = 0
        self.operandPrefix = 0

class DisAssembler:
    def __init__(self):
        self.ins = Instructions()
        self.pointer = 0
    
    def testing(self):
        print('operands')
        print(self.ins.operands)
        print('operands_len')
        print(self.ins.operands_len)
        print('prefix')
        print(self.ins.prefix)
        print('rex')
        print(self.ins.rex)
        print('rex_w')
        print(self.ins.rex_w)
        print('rex_r')
        print(self.ins.rex_r)
        print('rex_x')
        print(self.ins.rex_x)
        print('rex_b')
        print(self.ins.rex_b)
        print('opcode')
        print(self.ins.opcode)
        print('opcode_size')
        print(self.ins.opcode_size)
        print('d')
        print(self.ins.d)
        print('w')
        print(self.ins.w)
        print('mod')
        print(self.ins.mod)
        print('reg')
        print(self.ins.reg)
        print('rm')
        print(self.ins.rm)
        print('memory')
        print(self.ins.memory)
        print('sib')
        print(self.ins.sib)
        print('base')
        print(self.ins.base)
        print('index')
        print(self.ins.index)
        print('scale')
        print(self.ins.scale)
        print('disp')
        print(self.ins.disp)
        print('disp_size')
        print(self.ins.disp_size)
        print('imd')
        print(self.ins.imd)
        print('imd_size')
        print(self.ins.imd_size)
        print('opr_size')
        print(self.ins.opr_size)
        print('addr_size')
        print(self.ins.addr_size)
        print('addrPrefix')
        print(self.ins.addrPrefix)
        print('operandPrefix')
        print(self.ins.operandPrefix)
    def debug(self):
        a = []
        print(a[0])

    def getInstructionLen(self,code):
        length = 0
        while code != 0:
            code = code >> 4
            length += 1
        return length
    def get(self,start,end):
        string = self.str_code[start:end]

        return int(string,16)
    def get_str(self,start,end):
        return self.str_code[start:end]
    def checkPrefix(self):
        byte = self.get(0,2)
        if byte == 0x67:
            self.ins.addrPrefix = 1
            self.ins.prefix = 0x67
            self.pointer += 2
            byte = self.get(2,4)

        if byte == 0x66:
            self.ins.operandPrefix = 1
            self.ins.prefix <<= 8
            self.ins.prefix += 0x66
            self.pointer += 2
        
    def checkRex(self):
        byte = self.get(self.pointer,self.pointer+1)
        if byte == 0x4:
            self.ins.rex = 0x4
            rexx = self.get(self.pointer+1,self.pointer+2)
            self.ins.rex_w = (rexx & 0b1000) >> 3
            self.ins.rex_r = (rexx & 0b0100) >> 2
            self.ins.rex_x = (rexx & 0b0010) >> 1
            self.ins.rex_b = (rexx & 0b0001) >> 0

            self.pointer += 2
    def set_size(self):
        p67 = self.ins.addrPrefix
        p66 = self.ins.operandPrefix
        rex_w = self.ins.rex_w
        w = self.ins.w
        
        if p67:
            self.ins.addr_size = 32
        else:
            self.ins.addr_size = 64
        
        if p66:
            self.ins.opr_size = 16
        else:
            if rex_w:
                self.ins.opr_size = 64
            else:
                if w:
                    self.ins.opr_size = 32
                else:
                    self.ins.opr_size = 8



    def set_scale(self,ss):
        if ss == 0b00:
            self.ins.scale = 1
        elif ss == 0b01:
            self.ins.scale = 2
        elif ss == 0b10:
            self.ins.scale = 4
        elif ss == 0b11:
            self.ins.scale = 8

    def checkNoneOperand(self):
        for op in operators:
            if op['opcode'] == self.code:
                print(op['name'])                
                return True
                
    def UnaryUsuallOperator(self):
        
        myOpcode = self.get(self.pointer,self.pointer+2) >> 2
        myReg = (self.get(self.pointer+2,self.pointer+4) >> 3) & 0b111
        self.ins.opcode = myOpcode
        self.ins.reg = myReg
        self.ins.opcode_size = 6
        self.ins.d = 1
        self.ins.operands_len = 1
        for op in operators:
            opcode = op['opcode']
            
            try:
                reg = op['rcode']
            except:
                continue

            if opcode == myOpcode and reg == myReg:
                self.ins.operator = op
                self.ins.w = (self.get(self.pointer+1,self.pointer+2) & 0b1)
                self.pointer += 2
                self.set_size()
                self.processUnaryOperands()
                break

            
    def printt(self):
        if len(self.ins.operands) == 2:
            if self.ins.operator['name'] == 'xchg' or (self.ins.operator['name'] == 'bsf' or self.ins.operator['name'] == 'bsr') and not self.ins.memory:
                self.ins.operands[0],self.ins.operands[1] = self.ins.operands[1],self.ins.operands[0]   
        result = ''
        result += self.ins.operator['name']
        result += ' '
        if self.ins.operands_len == 1:
            if self.ins.memory:
                result += memory_size[self.ins.opr_size]
                result += ' PTR '
                result += '['
                if self.ins.sib:
                    try:
                        result += self.ins.base['name']
                        try:
                            result += '+'
                            result += self.ins.index['name']
                            result += '*'
                            result += str(self.ins.scale)
                            try:
                                if self.ins.disp_size > 0:
                                    result += '+'
                                    result +=  hex(self.ins.disp)
                            except:
                                pass
                        except:
                            if self.ins.disp_size > 0:
                                    result += '+'
                                    result +=  hex(self.ins.disp)
                    except:
                        try:
                            result += self.ins.index['name']
                            result += '*'
                            result += str(self.ins.scale)
                            try:
                                if self.ins.disp_size > 0:
                                    result += '+'
                                    result +=  hex(self.ins.disp)
                            except:
                                pass
                        except:
                            result +=  hex(self.ins.disp)

                else:
                    try:
                        result += self.ins.operands[0]['name']
                        if self.ins.disp_size > 0:
                            result += '+'
                            result +=  hex(self.ins.disp)
                    except:
                        result +=  hex(self.ins.disp)
                result += ']'
            else:
                result += self.ins.operands[0]['name']

            if self.ins.operator['name'] == 'shr' or self.ins.operator['name'] == 'shl':
                result += ',1'
            # self.testing()
            print(result)

        if self.ins.operands_len == 2:
            op1 = self.ins.operands[0]['name']
            op2 = ''
            if self.ins.memory:
                op2 += memory_size[self.ins.opr_size]
                op2 += ' PTR '
                op2 += '['
                if self.ins.sib:
                    try:
                        op2 += self.ins.base['name']
                        try:
                            index = self.ins.index['name']
                            op2 += '+'
                            op2 += self.ins.index['name']
                            op2 += '*'
                            op2 += str(self.ins.scale)
                            try:
                                if self.ins.disp_size > 0:
                                    op2 += '+'
                                    op2 +=  hex(self.ins.disp)
                            except:
                                pass
                        except:
                            if self.ins.disp_size > 0:
                                op2 += '+'
                                op2 +=  hex(self.ins.disp)

                    except:
                        try:
                            op2 += self.ins.index['name']
                            op2 += '*'
                            op2 += str(self.ins.scale)
                            try:
                                if self.ins.disp_size > 0:
                                    op2 += '+'
                                    op2 +=  hex(self.ins.disp)
                            except:
                                pass
                        except:
                            op2 +=  hex(self.ins.disp)

                else:
                    try:
                        op2 += self.ins.operands[1]['name']
                        if self.ins.disp_size > 0:
                            op2 += '+'
                            op2 +=  hex(self.ins.disp)
                    except:
                        op2 +=  hex(self.ins.disp)
                op2 += ']'
            else:
                op2 += self.ins.operands[1]['name']
            
            if self.ins.d == 0:
                op1, op2 = op2, op1

            if (self.ins.operator['name'] == 'bsf' or self.ins.operator['name'] == 'bsr') and self.ins.memory:
                op1, op2 = op2, op1
            result += op1
            result += ','
            result += op2
            print(result)

    def getDisp(self):
        string = str(hex(self.code))[2:]

        # string = self.str_code
        # print(string)
        if self.str_code[0] == '0':
            string = '0'+string
        string = string[self.pointer:]
        self.ins.disp_size = len(string) * 4
        disp = ['' for i in range(len(string)//2)]
        for i in range(len(string)//2):
            disp[-i-1] = string[i*2:i*2+2]
        string = ''
        for i in range(len(disp)):
            string += disp[i]
        if self.ins.disp_size > 0:
            self.ins.disp = int(string,16)
        # print(int(string,16))
        # return int(string,16)

    def processUnaryOperands(self):
        # if self.ins.operator['name'] == 'jmp':
        #     self.ins.opr_size *=2
        mod = self.get(self.pointer,self.pointer+1) >> 2
        rm = (self.get(self.pointer+1,self.pointer+2) & 0b111)
        # print(mod)
        # self.debug()
        if self.ins.operator['name'] == 'push' or self.ins.operator['name'] == 'pop':
            self.ins.opr_size = 64

        if mod == 0b11:
            self.ins.memory = False
            self.ins.sib = False
            for reg in registers:
                if reg['code'] == (rm+(self.ins.rex_b << 3)) and reg['size'] == self.ins.opr_size:
                    self.ins.operands.append(reg)
                    # print('hah')
                    break
        
        elif rm == 0b100:

            self.ins.memory = True
            self.ins.sib = True



            base = self.get(self.pointer+2,self.pointer+4) & 0b111
            index = (self.get(self.pointer+2,self.pointer+4) >> 3)& 0b111
            self.set_scale(self.get(self.pointer+2,self.pointer+4) >> 6)

            self.pointer += 4
                self.disp = self.getDisp()


            if base == 0b101:
                self.ins.base = {}
                
            else:
                for reg in registers:
                    if reg['code'] == (base+(self.ins.rex_b << 3)) and reg['size'] == self.ins.addr_size:
                        self.ins.base = reg
                        break
            # if index == 0b100:
            #     self.ins.index = {}
            # else:

            for reg in registers:
                if reg['code'] == (index+(self.ins.rex_x << 3)) and reg['size'] == self.ins.addr_size:
                    self.ins.index = reg
                    break
        else:
            self.ins.memory = True
            self.ins.sib = False
            # print(bin(self.ins.rex_b << 3))
            # print(bin(rm))
            for reg in registers:
                if reg['code'] == (rm+(self.ins.rex_b << 3)) and reg['size'] == self.ins.addr_size:
                    self.ins.operands.append(reg)
                    break
            # self.testing()
            self.pointer += 2
            self.getDisp()

        # self.testing()
        self.printt()
    def binary(self):
        myOpcode = self.get(self.pointer,self.pointer+4) >> 2
        # print(hex(myOpcode))
        # myOpcode = int(myOpcode,16) 

        # print(hex(myOpcode))
        for op in operators:
            opcode = op['opcode']
            if opcode == myOpcode:
                self.ins.operator = op
                self.ins.opcode = opcode
                self.ins.d = (self.get(self.pointer+3,self.pointer+4)>>1) & 0b1
                self.ins.w = self.get(self.pointer+3,self.pointer+4) & 0b1
                self.ins.opcode_size = 14
                self.ins.operands_len = 2
                self.pointer += 4
                if op['name'] == 'bsf':
                    if self.ins.w == 1:
                        self.pointer -= 4

                        continue
                    else:
                        break
                else:
                    break
        if self.ins.operator['name'] == 'bsf' or self.ins.operator['name'] == 'bsr':
            self.ins.w = 1
        # if self.ins.operator['name'] == 'bsf':
            # self.debug()
        self.set_size()

        self.processBinaryOperands()
    def processBinaryOperands(self):
        reg = ((self.get(self.pointer,self.pointer+2) >> 3) & 0b111) + (self.ins.rex_r << 3)
        for regs in registers:
            if regs['code'] == reg and regs['size'] == self.ins.opr_size:
                self.ins.operands.append(regs)
                break
        mod = self.get(self.pointer,self.pointer+1) >> 2
        rm = (self.get(self.pointer+1,self.pointer+2) & 0b111)



        if mod == 0b11:
            self.ins.memory = False
            self.ins.sib = False
            
            for reg in registers:
                if reg['code'] == (rm+(self.ins.rex_b << 3)) and reg['size'] == self.ins.opr_size:
                    self.ins.operands.append(reg)
                    break
        
        elif rm == 0b100:
            self.ins.memory = True
            self.ins.sib = True
            base = self.get(self.pointer+2,self.pointer+4) & 0b111
            index = (self.get(self.pointer+2,self.pointer+4) >> 3)& 0b111
            self.set_scale(self.get(self.pointer+2,self.pointer+4) >> 6)

            self.pointer += 4
            self.disp = self.getDisp()

            # if self.ins.addr_size > 32:
            # self.debug()            

            if base == 0b101:
                self.ins.base = {}
                
            else:
                for reg in registers:
                    if reg['code'] == (base+(self.ins.rex_b << 3)) and reg['size'] == self.ins.addr_size:
                        self.ins.base = reg
                        break
            # if index == 0b100:
            #     self.ins.index = {}
            # else:

            for reg in registers:
                if reg['code'] == (index+(self.ins.rex_x << 3)) and reg['size'] == self.ins.addr_size:
                    self.ins.index = reg
                    break
        else:
            self.ins.memory = True
            self.ins.sib = False
            # print(bin(self.ins.rex_b << 3))
            # print(bin(rm))
            for reg in registers:
                if reg['code'] == (rm+(self.ins.rex_b << 3)) and reg['size'] == self.ins.addr_size:
                    self.ins.operands.append(reg)
                    break
            self.pointer += 2
            self.getDisp()
        # self.testing()
        self.printt()
    def reg_mem(self):
        myOpcode = self.get(self.pointer,self.pointer+2) >> 2
        if self.ins.d == 0:
            pass
        for op in operators:
            opcode = op['opcode']
            if opcode == myOpcode:
                self.ins.operator = op
                self.ins.opcode = opcode
                self.ins.d = (self.get(self.pointer+1,self.pointer+2)>>1) & 0b1
                self.ins.w = self.get(self.pointer+1,self.pointer+2) & 0b1
                self.ins.opcode_size = 6
                self.ins.operands_len = 2
                self.pointer += 2
                self.set_size()
                if self.ins.operator['name'] == 'test':
                    if self.ins.d == 1:
                        self.pointer -= 2

                        continue
                    else:
                        break

                if op['name'] == 'test' or op['name'] == 'xchg':
                    self.ins.d = 1                  
                break
            
        self.processRegMemOperands()
    def processRegMemOperands(self):

        reg = ((self.get(self.pointer,self.pointer+2) >> 3) & 0b111) + (self.ins.rex_r << 3)
        for regs in registers:
            if regs['code'] == reg and regs['size'] == self.ins.opr_size:
                self.ins.operands.append(regs)
                break
        mod = self.get(self.pointer,self.pointer+1) >> 2
        rm = (self.get(self.pointer+1,self.pointer+2) & 0b111)

        if mod == 0b11:
            self.ins.memory = False
            self.ins.sib = False


            for reg in registers:
                if reg['code'] == (rm+(self.ins.rex_b << 3)) and reg['size'] == self.ins.opr_size:
                    self.ins.operands.append(reg)
                    break
        


        elif rm == 0b100:

            self.ins.memory = True
            self.ins.sib = True



            base = self.get(self.pointer+2,self.pointer+4) & 0b111
            index = (self.get(self.pointer+2,self.pointer+4) >> 3)& 0b111
            self.set_scale(self.get(self.pointer+2,self.pointer+4) >> 6)

            self.pointer += 4
            self.disp = self.getDisp()


            if base == 0b101 and self.ins.disp_size != 8:
                self.ins.base = {}
                
            else:
                for reg in registers:
                    if reg['code'] == (base+(self.ins.rex_b << 3)) and reg['size'] == self.ins.addr_size:
                        self.ins.base = reg
                        break
            # if index == 0b100:
            #     self.ins.index = {}
            # else:
                # self.debug()

            for reg in registers:
                if reg['code'] == (index+(self.ins.rex_x << 3)) and reg['size'] == self.ins.addr_size:
                    self.ins.index = reg
                    break
        else:
            self.ins.memory = True
            self.ins.sib = False
            # print(bin(self.ins.rex_b << 3))
            # print(bin(rm))
            for reg in registers:
                if reg['code'] == (rm+(self.ins.rex_b << 3)) and reg['size'] == self.ins.addr_size:
                    self.ins.operands.append(reg)
                    break
            self.pointer += 2
            self.getDisp()
        # self.testing()
        self.printt()
        pass
    def imd_reg(self):
        myOpcode = self.get(self.pointer,self.pointer+2) >> 2
        myReg = (self.get(self.pointer+2,self.pointer+4) >> 3) & 0b111
        
        # print(myOpcode)
        # print(myReg)

        self.ins.opcode = myOpcode
        self.ins.reg = myReg
        self.ins.opcode_size = 6
        self.ins.d = 1
        self.ins.operands_len = 2
        for op in operators:
            try:
                opcode = op['iopcode'] >> 3
                reg = op['iopcode'] & 0b111
            except:
                continue
            # print('#')
            # print(hex(opcode))
            # print(hex(myOpcode))
            # print('--')
            # print(hex(reg))
            # print(hex(myReg))

            if opcode == myOpcode and reg == myReg:
                # print('hah')
                self.ins.operator = op
                self.ins.w = (self.get(self.pointer+1,self.pointer+2) & 0b1)
                self.pointer += 2
                self.set_size()
                break
        self.imdOperand()

    def setImd(self):

        string = str(hex(self.code))[2:]
        string = string[self.pointer+2:]
        self.ins.disp_size = len(string) * 4
        disp = ['' for i in range(len(string)//2)]
        for i in range(len(string)//2):
            disp[-i-1] = string[i*2:i*2+2]
        string = ''
        for i in range(len(disp)):
            string += disp[i]
        if self.ins.disp_size > 0:
            self.ins.disp = int(string,16)
        self.ins.operands.append({'name':hex(self.ins.disp)})
    def imdOperand(self):

        mod = self.get(self.pointer,self.pointer+1) >> 2
        rm = (self.get(self.pointer+1,self.pointer+2) & 0b111)

        if mod == 0b11:
            self.ins.memory = False
            self.ins.sib = False
            for reg in registers:
                if reg['code'] == (rm+(self.ins.rex_b << 3)) and reg['size'] == self.ins.opr_size:
                    self.ins.operands.append(reg)
                    break
            self.setImd()
            # self.debug()
        
        elif rm == 0b100:
            self.ins.memory = True
            self.ins.sib = True

            base = self.get(self.pointer+2,self.pointer+4) & 0b111
            index = (self.get(self.pointer+2,self.pointer+4) >> 3)& 0b111
            self.set_scale(self.get(self.pointer+2,self.pointer+4) >> 6)

            self.pointer += 4
            self.disp = self.getDisp()

            if base == 0b101:
                self.ins.base = {}
                
            else:
                for reg in registers:
                    if reg['code'] == (base+(self.ins.rex_b << 3)) and reg['size'] == self.ins.addr_size:
                        self.ins.base = reg
                        break
            # if index == 0b100:
            #     self.ins.index = {}
            # else:

            for reg in registers:
                if reg['code'] == (index+(self.ins.rex_x << 3)) and reg['size'] == self.ins.addr_size:
                    self.ins.index = reg
                    break
        else:
            self.ins.memory = True
            self.ins.sib = False

            for reg in registers:
                if reg['code'] == (rm+(self.ins.rex_b << 3)) and reg['size'] == self.ins.addr_size:
                    self.ins.operands.append(reg)
                    break
            self.pointer += 2
            self.getDisp()

        self.printt()

    def procces(self,bincode):
        # return 0
        code = int(bincode,16)
        self.code = code
        self.str_code = bincode
        self.ins.len = self.getInstructionLen(code)
        self.pointer = 0


        if self.checkNoneOperand():
            return

        self.checkPrefix()
        self.checkRex()


        

        if self.get(self.pointer,self.pointer+2) == 0xc2:
            self.pointer += 2
            self.getDisp()
            print('ret '+ str(hex(self.ins.disp)))

        elif self.get(self.pointer,self.pointer+3) >> 3 == 0b01010:
            self.ins.operator = {'name':'push'}
            print(self.get(self.pointer,self.pointer+3))
            for reg in registers:
                rm = self.get(self.pointer+1,self.pointer+2) & 0b111
                if reg['code'] == (rm+(self.ins.rex_b << 3)) and reg['size'] == 64:
                    self.ins.operands.append(reg)
                    self.ins.operands_len = 1
                    break
            self.printt()
        elif self.get(self.pointer,self.pointer+3) >> 3 == 0b01011:
            self.ins.operator = {'name':'pop'}
            for reg in registers:
                rm = self.get(self.pointer+1,self.pointer+2) & 0b111
                if reg['code'] == (rm+(self.ins.rex_b << 3)) and reg['size'] == 64:
                    self.ins.operands.append(reg)
                    self.ins.operands_len = 1
                    break
            self.printt()
        else:
            next_char = self.get(self.pointer,self.pointer+1)
            if (next_char == 0xf) or (next_char == 0x8 and self.get(self.pointer+1,self.pointer+2) == 0xf) or next_char == 0xd or (next_char == 0xc and self.get(self.pointer+1,self.pointer+2)==0x4):
                self.UnaryUsuallOperator()

            elif next_char == 0x0 and self.get(self.pointer+1,self.pointer+2) == 0xf:
                self.binary()

            elif (next_char < 4) or (next_char == 8 and (self.get(self.pointer+1,self.pointer+2)>>2) != 0):
                self.reg_mem()

            elif (next_char == 0x8) or (next_char == 0xc) or (next_char == 0xf):
                self.imd_reg()
                pass

        







if __name__ == '__main__':
    test1 = ['ff30',
                      
    ]
    # for i,t in enumerate(test1):
    #     print(t+': ')
    #     dis = DisAssembler()
    #     dis.procces(t)
        # print('########################')

    dis = DisAssembler()
    dis.procces(input())

