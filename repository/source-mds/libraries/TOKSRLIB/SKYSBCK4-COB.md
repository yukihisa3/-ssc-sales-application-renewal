# SKYSBCK4

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SKYSBCK4.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　販売管理システム　　　　　　　　　　　*
*    モジュール名　　：　納品管理番号Ｃ/Ｄ計算サブルーチン　　
*    　　　　　　　　　　IN:２１桁　モジュラス１０　ウェイト３
*    作成日／更新日　：　2019/07/04                            *
*    作成者／更新者　：　NAV                                   *
*                        LO-ERR    :  9:区分，桁数指定エラー   *
*                                     3:非数値エラー　　       *
*                                    X2:伝票_範囲エラー       *
*                                    ?1:チェックデジットエラー *
*                                     0:正常終了               *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKYSBCK4.
*                  流用:SKYSBCK1
 AUTHOR.                N.K.
 DATE-WRITTEN.          2019/07/04.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
 INPUT-OUTPUT           SECTION.
******************************************************************
 DATA                      DIVISION.
******************************************************************
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WORK-AREA.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  WK-CD               PIC  9(04).
     03  WK-CALC             PIC  9(01).
     03  WK-CALC-2           PIC  9(02).
*    納品管理番号（除C/D)
     03  WK-NO               PIC  X(21).
*    納品管理番号（C/D)算出値
     03  CALC-CD             PIC  9(01).
*
     03  WK-DIV-1            PIC  9(03).
     03  WK-DIV-2            PIC  9(03).
*    03  WK-KETA             PIC  9(01).
*    03  WK-NEXT.
*        05  WK-NEXT-9       PIC  9(09).
*    03  WK-POS              PIC  9(02).
*    03  WK-LEN              PIC  9(02).
*    03  WK-DENCHK.
*        05  WK-DENCHK1      PIC  9(08).
*        05  WK-DENCHK2      PIC  9(01).
*
     03  WK-DIVW3-1          PIC  9(03).
     03  WK-DIVW3-2          PIC  9(03).
     03  WK-KETAW            PIC  9(01).
*
 01  SEC-NAME                PIC  X(30).
*
 01  WK-PARA.
     03  PR-KBN              PIC  9.
     03  PR-KETA             PIC  9.
     03  PR-START            PIC  9(09).
     03  PR-END              PIC  9(09).

*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-CODE         PIC  X(21).
     03  LINK-OUT.
         05  LO-ERR          PIC  9(01).
         05  LO-CODE         PIC  X(22).
*
******************************************************************
 PROCEDURE                 DIVISION    USING     LINK-AREA.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE "PROG-CNTL"  TO    SEC-NAME.
     MOVE     ZERO     TO    LO-ERR.
     PERFORM  100-CHECK-PARA.
     IF       LO-ERR   =   0
              PERFORM      300-CHECK-CD
     END-IF.
*
     EXIT     PROGRAM.
*
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      パラメタチェック                            *
*--------------------------------------------------------------*
 100-CHECK-PARA         SECTION.
     MOVE     "100-CHECK-PARA" TO   SEC-NAME.
*
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  21
              MOVE     LI-CODE (I:1)  TO   WK-CALC
              IF   WK-CALC NOT NUMERIC
                   MOVE      3         TO   LO-ERR
                   GO   TO   100-CHECK-PARA-EXIT
              END-IF
     END-PERFORM.
*
 100-CHECK-PARA-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      チェックデジットつきの場合のチェック        *
*--------------------------------------------------------------*
 300-CHECK-CD           SECTION.
*
     MOVE     "300-CHECK-CD"      TO    SEC-NAME.
*

     PERFORM  320-CALC-MOD10W3.
*
     MOVE     LI-CODE             TO    LO-CODE(1:21).
     MOVE     CALC-CD             TO    LO-CODE(22:1).
*
 300-CHECK-CD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス１０（ウエイト３）の計算　　　    *
*--------------------------------------------------------------*
 320-CALC-MOD10W3       SECTION.
*
     MOVE     "320-CALC-MOD10W3"  TO  SEC-NAME.
*
     MOVE     0              TO   WK-CD.
     MOVE     1              TO   J.
*
     PERFORM  VARYING  I  FROM  1   BY   1    UNTIL  I  >  21
              MOVE     LI-CODE (I:1)     TO   WK-CALC
              IF   J   =  0
                   ADD    WK-CALC        TO   WK-CD
                   MOVE   1              TO   J
              ELSE
                   COMPUTE  WK-CALC-2    =    WK-CALC  *  3
                   COMPUTE  WK-CD        =    WK-CD  +  WK-CALC-2
                   MOVE     0            TO   J
              END-IF
     END-PERFORM.
*
*    DISPLAY "WK-CD=" WK-CD UPON CONS.
     IF      WK-CD  =  ZERO
             MOVE      WK-CD             TO   CALC-CD
     ELSE
             DIVIDE    WK-CD  BY  10   GIVING    WK-DIV-1
                                       REMAINDER WK-DIV-2
             COMPUTE   CALC-CD =  10 - WK-DIV-2
     END-IF.
*
 320-CALC-MOD10W3-EXIT.
     EXIT.

```
