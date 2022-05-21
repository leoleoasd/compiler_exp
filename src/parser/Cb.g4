grammar Cb;
options {
    tokenVocab = CbLexer ; 
}
unit: BLOCK_COMMENT | LINE_COMMENT;
