# SSKTLSTD

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSKTLSTD.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　販売管理システム　　　　　　　　　　　*
*    モジュール名　　：　月末日取得サブルーチン　　　　　　　　*
*    作成日／更新日　：　92/12/05                              *
*    作成者／更新者　：　NAV                                   *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            OSKTLSTD.
 AUTHOR.                S.K.
 DATE-WRITTEN.          92/12/05.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
***************************************************************
 INPUT-OUTPUT           SECTION.
******************************************************************
 DATA                      DIVISION.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(02).
     03  CHK-02              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
 01  INDEXES.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
 01  SSKTLSTD-YMD.
     03  SSKTLSTD-Y          PIC  9(04).
     03  SSKTLSTD-M          PIC  9(02).
     03  SSKTLSTD-D          PIC  9(02).
 01  SSKTLSTD-RET            PIC  9(01).
*
******************************************************************
 PROCEDURE    DIVISION       USING     SSKTLSTD-YMD
                                       SSKTLSTD-RET.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE     0         TO   SSKTLSTD-RET.
     DIVIDE   SSKTLSTD-Y     BY   4
              GIVING    CHK-01    REMAINDER      CHK-02.
     IF       CHK-02    =    0
              MOVE      29        TO   WK-MATUBI (2)
     ELSE
              MOVE      28        TO   WK-MATUBI (2)
     END-IF.
     IF       SSKTLSTD-M   =  0   OR   >    12
              MOVE      9         TO   SSKTLSTD-RET
     ELSE
              MOVE      WK-MATUBI (SSKTLSTD-M)    TO   SSKTLSTD-D
     END-IF.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*-----------------<< PROGRAM END >>----------------------------*

```
