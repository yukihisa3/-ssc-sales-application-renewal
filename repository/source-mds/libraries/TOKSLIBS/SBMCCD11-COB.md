# SBMCCD11

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBMCCD11.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　販売管理システム　　　　　　　　　　　*
*    モジュール名　　：　モジュラス１１ＣＤ算出　　　　　　　　*
*    作成日／更新日　：　2013/03/18                            *
*    作成者／更新者　：　NAV TAKEI                             *
*                        LO-ERR    :  0:正常終了               *
*                                     1:チェックデジットエラー *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBMCCD11.
 AUTHOR.                NAV TAKEI.
 DATE-WRITTEN.          13/03/18.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
******************************************************************
 DATA                      DIVISION.
******************************************************************
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WORK-AREA.
     03  WK-CD               PIC  9(04).
     03  WK-CALC             PIC  9(01).
     03  WK-CALC-2           PIC  9(02).
     03  WK-DIV-1            PIC  9(03).
     03  WK-DIV-2            PIC  9(03).
     03  WK-KETA             PIC  9(02).
     03  WKDATA-KETA         PIC  9(01).
     03  CALC-CD             PIC  9(01).
     03  WK-DENNO            PIC  9(09).
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).

*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
***  LINK-AREA.
*******  LINK-IN.
 01  LI-DATA         PIC  9(09).
*******  LINK-OUT.
 01  LO-CD           PIC  9(01).
 01  LO-ERR          PIC  9(01).
*
******************************************************************
 PROCEDURE                 DIVISION    USING
                           LI-DATA  LO-CD  LO-ERR.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     INITIALIZE         LO-CD  LO-ERR.
     PERFORM  100-CHECK-PARA.
     IF    LO-ERR  =  0
           PERFORM   300-CALC-MOD11W
           MOVE    CALC-CD  TO  LO-CD
     END-IF.
     EXIT     PROGRAM.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      パラメタチェック                            *
*--------------------------------------------------------------*
 100-CHECK-PARA         SECTION.
     IF       LI-DATA  NOT  NUMERIC
              MOVE      1         TO   LO-ERR
              GO   TO   100-CHECK-PARA-EXIT
     END-IF.
*
 100-CHECK-PARA-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス１１の計算  2-7 W                 *
*--------------------------------------------------------------*
 300-CALC-MOD11W        SECTION.
     MOVE     LI-DATA        TO   WK-DENNO.
     MOVE     0              TO   WK-CD.
     MOVE     9              TO   WKDATA-KETA.
     COMPUTE  WK-KETA  =  10  -  WKDATA-KETA.
     MOVE     10          TO   WK-KETA.
     PERFORM  VARYING  I  FROM  1        BY  1  UNTIL  I  >  9
              MOVE      WK-DENNO (WK-KETA - I:1) TO  WK-CALC
        IF  I  >  6
              COMPUTE  WK-CD  =  WK-CD  +
                          (WK-CALC  *  ( I -  6  +  1))
***           DISPLAY  "WK-CALC="  WK-CALC  " WK-CD="  WK-CD
***                    " I="   I   UPON CONS
        ELSE
              COMPUTE  WK-CD  =  WK-CD  +
                          (WK-CALC  *  ( I +  1 ))
        END-IF
     END-PERFORM.
*
     DIVIDE   WK-CD  BY  11  GIVING    WK-DIV-1
                             REMAINDER WK-DIV-2.
     IF  WK-DIV-2  =  0  OR  1
         MOVE   0   TO   CALC-CD
     ELSE
         COMPUTE  CALC-CD   =  11  -  WK-DIV-2
     END-IF.
**
 300-CALC-MOD11W-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス１１の計算  X                     *
*--------------------------------------------------------------*
 300-CALC-MOD11X        SECTION.
     MOVE     LI-DATA        TO   WK-DENNO.
     MOVE     0              TO   WK-CD.
     MOVE     0              TO   J.
     MOVE     9              TO   WKDATA-KETA.
     COMPUTE  WK-KETA  =  10  -  WKDATA-KETA.
     MOVE     10          TO   WK-KETA.
     PERFORM  VARYING  I  FROM  1        BY  1  UNTIL  I  >  9
              MOVE      WK-DENNO (WK-KETA - I:1) TO  WK-CALC
                   COMPUTE  WK-CALC-2  =  WK-CALC  *  1
                   COMPUTE  WK-CD = WK-CD + WK-CALC-2
     END-PERFORM.
*
     DIVIDE   WK-CD  BY  11  GIVING    WK-DIV-1
                             REMAINDER WK-DIV-2.
     IF  WK-DIV-2  =  0  OR  1
         MOVE   0   TO   CALC-CD
     ELSE
         COMPUTE  CALC-CD   =  11  -  WK-DIV-2
     END-IF.
 300-CALC-MOD11X-EXIT.
     EXIT.

```
