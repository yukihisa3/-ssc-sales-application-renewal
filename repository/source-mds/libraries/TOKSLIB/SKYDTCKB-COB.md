# SKYDTCKB

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKYDTCKB.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　：　_サカタのタネ　特販部　殿向け　　　　*
*    業務名　　　　　：　システム共通サブシステム　　　　　　　*
*    モジュール名　　：　日付チェック／８桁変換                *
*    作成日／更新日　：　1999/09/03                            *
*    作成者／更新者　：　NAV-ASSIST                            *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKYDTCKB.
 AUTHOR.                NAV.
 DATE-WRITTEN.          99/09/03.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
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
 01  WK-NEN                  PIC  9(04)  VALUE  ZERO.
 01  WK-HIDUKE               PIC  9(02)  VALUE  ZERO.
 01  WK-YMD.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
 01  WK-H-YMD.
     03  WK-H-Y              PIC  9(04)  VALUE  ZERO.
     03  WK-H-M              PIC  9(02)  VALUE  ZERO.
     03  WK-H-D              PIC  9(02)  VALUE  ZERO.
 01  WK-DAY                  PIC S9(02)  VALUE  ZERO.
*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
 01  IN-KBN                  PIC  X(01).
 01  IN-YMD1.
     03  IN-YMD1-YY          PIC  9(02).
     03  IN-YMD1-MM          PIC  9(02).
     03  IN-YMD1-DD          PIC  9(02).
 01  IN-YMD2.
     03  IN-YMD2-YY          PIC  9(04).
     03  IN-YMD2-MM          PIC  9(02).
     03  IN-YMD2-DD          PIC  9(02).
 01  OUT-RET                 PIC  X(01).
 01  OUT-YMD.
     03  OUT-YMD-YYYY        PIC  9(04).
     03  OUT-YMD-MD          PIC  9(02).
     03  OUT-YMD-DD          PIC  9(02).
*
******************************************************************
 PROCEDURE    DIVISION       USING     IN-KBN IN-YMD1 IN-YMD2
                                       OUT-RET OUT-YMD.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*    DISPLAY "IN-KBN  S = " IN-KBN   UPON CONS.
*    DISPLAY "IN-YMD1 S = " IN-YMD1  UPON CONS.
*    DISPLAY "IN-YMD2 S = " IN-YMD2  UPON CONS.
*    DISPLAY "OUT-RET S = " OUT-RET  UPON CONS.
*    DISPLAY "OUT-YMD S = " OUT-YMD  UPON CONS.
     MOVE     0         TO   OUT-RET.
     EVALUATE   IN-KBN
         WHEN    "1"    PERFORM DATE-6-CHK
         WHEN    "2"    PERFORM DATE-8-CHK
         WHEN    "3"    PERFORM DATE-SEIREKI
                        PERFORM DATE-8-CHK
                        MOVE    IN-YMD2    TO  OUT-YMD
         WHEN    "4"    PERFORM DATE-WAREKI
                        PERFORM DATE-8-CHK
                        MOVE    IN-YMD2    TO  OUT-YMD
         WHEN    "5"    PERFORM DATE-PLUS-SEC
         WHEN    "6"    PERFORM DATE-MAINAS-SEC
         WHEN    OTHER  MOVE    9    TO    OUT-RET
     END-EVALUATE.
*    DISPLAY "IN-KBN  E = " IN-KBN   UPON CONS.
*    DISPLAY "IN-YMD1 E = " IN-YMD1  UPON CONS.
*    DISPLAY "IN-YMD2 E = " IN-YMD2  UPON CONS.
*    DISPLAY "OUT-RET E = " OUT-RET  UPON CONS.
*    DISPLAY "OUT-YMD E = " OUT-YMD  UPON CONS.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL   1.1   日付６桁　論理チェック                      *
*--------------------------------------------------------------*
 DATE-6-CHK             SECTION.
     DIVIDE   IN-YMD1-YY     BY   4
              GIVING    CHK-01    REMAINDER      CHK-02.
     IF       CHK-02    =    0
              MOVE      29        TO   WK-MATUBI (2)
     ELSE
              MOVE      28        TO   WK-MATUBI (2)
     END-IF.
     IF       IN-YMD1-MM   =  0   OR   >    12
              MOVE      9         TO   OUT-RET
     END-IF.
     IF       OUT-RET        =    0
     AND (    IN-YMD1-DD     =    0
         OR   IN-YMD1-DD     >    WK-MATUBI (IN-YMD1-MM)   )
              MOVE      9         TO   OUT-RET
     END-IF.
 DATE-6-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   日付８桁　論理チェック                      *
*--------------------------------------------------------------*
 DATE-8-CHK             SECTION.
     DIVIDE   IN-YMD2-YY     BY   4
              GIVING    CHK-01    REMAINDER      CHK-02.
     IF       CHK-02    =    0
              MOVE      29        TO   WK-MATUBI (2)
     ELSE
              MOVE      28        TO   WK-MATUBI (2)
     END-IF.
     IF       IN-YMD2-MM   =  0   OR   >    12
              MOVE      9         TO   OUT-RET
     END-IF.
     IF       OUT-RET        =    0
     AND (    IN-YMD2-DD     =    0
         OR   IN-YMD2-DD     >    WK-MATUBI (IN-YMD2-MM)   )
              MOVE      9         TO   OUT-RET
     END-IF.
 DATE-8-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   日付６桁→日付８桁変換                      *
*--------------------------------------------------------------*
 DATE-SEIREKI           SECTION.
*
     MOVE     IN-YMD1-YY   TO   IN-YMD2-YY.
     IF       IN-YMD1-YY  >  89
              ADD   1900   TO   IN-YMD2-YY
     ELSE
              ADD   2000   TO   IN-YMD2-YY
     END-IF.
     MOVE     IN-YMD1-MM   TO  IN-YMD2-MM.
     MOVE     IN-YMD1-DD   TO  IN-YMD2-DD.
*
 DATE-SEIREKI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   日付和暦－＞日付西暦変換                    *
*--------------------------------------------------------------*
 DATE-WAREKI            SECTION.
*
     MOVE     IN-YMD1-YY   TO   IN-YMD2-YY.
     ADD      1988         TO   IN-YMD2-YY.
     MOVE     IN-YMD1-MM   TO  IN-YMD2-MM.
     MOVE     IN-YMD1-DD   TO  IN-YMD2-DD.
*
 DATE-WAREKI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   Ｎ日後算出                                  *
*--------------------------------------------------------------*
 DATE-PLUS-SEC          SECTION.
*基準日付の退避
     MOVE     IN-YMD2           TO   WK-YMD.
*Ｎ日後の退避
     MOVE     IN-YMD1-DD        TO   WK-HIDUKE.
*西暦÷４（閏年チェック）
     DIVIDE   IN-YMD2-YY   BY   4    GIVING    CHK-01
                                     REMAINDER CHK-02.
     IF       CHK-02  =  0
              MOVE    29        TO   WK-MATUBI(2)
     ELSE
              MOVE    28        TO   WK-MATUBI(2)
     END-IF.
*Ｎ日加算
     COMPUTE  WK-D  =  WK-D  +  WK-HIDUKE.
*加算後日付が基準日付（日）末日より大きかったら
     IF       WK-D  >  WK-MATUBI(IN-YMD2-MM)
              ADD      1         TO    WK-M
              IF       WK-M  >  12
                       ADD   1   TO    WK-Y
                       MOVE  1   TO    WK-M
              END-IF
              COMPUTE  WK-D  =  WK-D  -  WK-MATUBI(IN-YMD2-MM)
              MOVE     WK-YMD    TO    OUT-YMD
     ELSE
              MOVE     WK-YMD    TO    OUT-YMD
     END-IF.
*
 DATE-PLUS-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   Ｎ日前算出                                  *
*--------------------------------------------------------------*
 DATE-MAINAS-SEC        SECTION.
*基準日付の退避
     MOVE     IN-YMD2           TO   WK-YMD.
*Ｎ日前の退避
     MOVE     IN-YMD1-DD        TO   WK-HIDUKE.
*西暦÷４（閏年チェック）
     DIVIDE   IN-YMD2-YY   BY   4    GIVING    CHK-01
                                     REMAINDER CHK-02.
     IF       CHK-02  =  0
              MOVE    29        TO   WK-MATUBI(2)
     ELSE
              MOVE    28        TO   WK-MATUBI(2)
     END-IF.
*Ｎ日減算
     COMPUTE  WK-DAY = WK-D  -  WK-HIDUKE.
*減算後日付が０以下だったら
     IF       WK-DAY  <= 0
              SUBTRACT 1         FROM  WK-M
              COMPUTE  WK-D  =   WK-MATUBI(WK-M) + WK-DAY
              IF       WK-M  <=  0
                       SUBTRACT  1     FROM   WK-Y
                       MOVE      12    TO     WK-M
              END-IF
              MOVE     WK-YMD    TO    OUT-YMD
     ELSE
              MOVE     WK-DAY    TO    WK-D
              MOVE     WK-YMD    TO    OUT-YMD
     END-IF.
*
 DATE-MAINAS-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
