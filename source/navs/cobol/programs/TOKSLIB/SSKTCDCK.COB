****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　販売管理システム　　　　　　　　　　　*
*    モジュール名　　：　伝票番号ＣＤチェック，次_作成　　　　*
*    作成日／更新日　：　92/11/20                              *
*    作成者／更新者　：　NAV                                   *
*                        LO-ERR    :  9:区分，桁数指定エラー   *
*                                     3:伝票_桁数エラー       *
*                                     2:伝票_範囲エラー       *
*                                     1:チェックデジットエラー *
*                                     0:正常終了               *
*    2001/08/03  モジュラス１０（ウエイト１．３）追加          *
*    2019/06/21  モジュラス１０（ウエイト３）追加　　　        *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            OSKTCDCK.
 AUTHOR.                N.K.
 DATE-WRITTEN.          92/11/19.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
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
     03  WK-KETA             PIC  9(01).
     03  CALC-CD             PIC  9(01).
     03  WK-NEXT.
         05  WK-NEXT-9       PIC  9(09).
     03  WK-DENNO            PIC  9(09).
     03  WK-POS              PIC  9(02).
     03  WK-LEN              PIC  9(02).
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  WK-DENCHK.
         05  WK-DENCHK1      PIC  9(08).
         05  WK-DENCHK2      PIC  9(01).
*#2019/06/18 NAV ST
     03  WK-DIVW3-1          PIC  9(03).
     03  WK-DIVW3-2          PIC  9(03).
     03  WK-KETAW            PIC  9(01).
*#2019/06/18 NAV ED

*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-KBN          PIC  9(01).
         05  LI-KETA         PIC  9(01).
         05  LI-START        PIC  9(09).
         05  LI-END          PIC  9(09).
         05  LI-DENNO        PIC  9(09).
     03  LINK-OUT.
         05  LO-ERR          PIC  9(01).
         05  LO-NEXT         PIC  9(09).
*
******************************************************************
 PROCEDURE                 DIVISION    USING     LINK-AREA.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     INITIALIZE              LINK-OUT.
     PERFORM  100-CHECK-PARA.
     IF       LO-ERR  =  0
              EVALUATE LI-KBN
                   WHEN     0
                        PERFORM   200-CHECK-NOCD
*#2019/06/18 NAV ST １～８に変更
*******************WHEN     1  THRU  7
                   WHEN     1  THRU  8
*#2019/06/18 NAV ED
                        PERFORM   300-CHECK-CD
                   WHEN     OTHER
                        MOVE      9         TO   LO-ERR
              END-EVALUATE
     END-IF.
     EXIT     PROGRAM.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      パラメタチェック                            *
*--------------------------------------------------------------*
 100-CHECK-PARA         SECTION.
     COMPUTE  WK-LEN  =  9  -  LI-KETA.
     IF       WK-LEN  NOT  =  0
     AND      LI-DENNO (1:WK-LEN)  NOT  =  ZERO
              MOVE      3         TO   LO-ERR
              GO   TO   100-CHECK-PARA-EXIT
     END-IF.
*
     IF       LI-DENNO  <  LI-START
     OR       LI-DENNO  >  LI-END
              MOVE      2         TO   LO-ERR
              GO   TO   100-CHECK-PARA-EXIT
     END-IF.
 100-CHECK-PARA-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      チェックデジットなしの場合のチェック        *
*--------------------------------------------------------------*
 200-CHECK-NOCD         SECTION.
     COMPUTE  LO-NEXT  =  LI-DENNO  +  1.
     IF       LO-NEXT  <  LI-START
     OR       LO-NEXT  >  LI-END
              MOVE      LI-START       TO   LO-NEXT
     END-IF.
 200-CHECK-NOCD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      チェックデジットつきの場合のチェック        *
*--------------------------------------------------------------*
 300-CHECK-CD           SECTION.
     MOVE     LI-DENNO       TO   WK-DENNO.
     EVALUATE LI-KBN
              WHEN  1   PERFORM  310-CALC-MOD7
              WHEN  2   PERFORM  320-CALC-MOD10
              WHEN  3   PERFORM  330-CALC-MOD11
              WHEN  4   PERFORM  330-CALC-MOD11P2
              WHEN  5   PERFORM  330-CALC-MOD11P3
              WHEN  6   PERFORM  310-CALC-MOD7X
              WHEN  7   PERFORM  320-CALC-MOD10P3
**************#2019/06/18 NAV ST
              WHEN  8   PERFORM  320-CALC-MOD10W3
**************#2019/06/18 NAV ED
     END-EVALUATE.
     IF       CALC-CD  NOT  =  LI-DENNO (9:1)
              MOVE      1         TO   LO-ERR
              GO   TO   300-CHECK-CD-EXIT
     END-IF.
     COMPUTE  LO-NEXT  =  LI-DENNO  +  10.
     MOVE     LO-NEXT        TO   WK-DENNO.
     EVALUATE LI-KBN
              WHEN  1   PERFORM  310-CALC-MOD7
              WHEN  2   PERFORM  320-CALC-MOD10
              WHEN  3   PERFORM  330-CALC-MOD11
              WHEN  4   PERFORM  330-CALC-MOD11P2
              WHEN  5   PERFORM  330-CALC-MOD11P3
              WHEN  6   PERFORM  310-CALC-MOD7X
              WHEN  7   PERFORM  320-CALC-MOD10P3
**************#2019/06/18 NAV ST
              WHEN  8   PERFORM  320-CALC-MOD10W3
**************#2019/06/18 NAV ED
     END-EVALUATE.
     MOVE     CALC-CD        TO   LO-NEXT (9:1).
     IF       LO-NEXT  <  LI-START
     OR       LO-NEXT  >  LI-END
              MOVE      LI-START       TO   LO-NEXT
     END-IF.
 300-CHECK-CD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス７の計算                          *
*--------------------------------------------------------------*
 310-CALC-MOD7X         SECTION.
     MOVE     0              TO   WK-CD.
     COMPUTE  WK-KETA  =  10  -  LI-KETA.
     MOVE     WK-DENNO       TO   WK-DENCHK.
     DIVIDE   WK-DENCHK1 BY  7    GIVING    WK-DIV-1
                                REMAINDER   WK-DIV-2
     COMPUTE  CALC-CD   =         WK-DIV-2.
 310-CALC-MOD7X-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス７の計算                          *
*--------------------------------------------------------------*
 310-CALC-MOD7          SECTION.
     MOVE     0              TO   WK-CD.
     COMPUTE  WK-KETA  =  10  -  LI-KETA.
     PERFORM  VARYING  I  FROM  WK-KETA  BY  1  UNTIL  I  >  8
              MOVE      WK-DENNO (I:1) TO   WK-CALC
              DIVIDE  I  BY  2  GIVING      WK-DIV-1
                                REMAINDER   WK-DIV-2
              IF   WK-DIV-2  =  0
                   ADD  WK-CALC        TO   WK-CD
              ELSE
                   COMPUTE  WK-CALC-2  =  WK-CALC  *  2
                   DIVIDE   WK-CALC-2  BY  10  GIVING     WK-DIV-1
                                               REMAINDER  WK-DIV-2
                   COMPUTE  WK-CD = WK-CD + WK-DIV-1 + WK-DIV-2
              END-IF
     END-PERFORM.
*
     DIVIDE   WK-CD  BY  7   GIVING    WK-DIV-1
                             REMAINDER WK-DIV-2.
     COMPUTE  CALC-CD   =  7   -  WK-DIV-2.
 310-CALC-MOD7-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス１０（ウエイト１・２）の計算      *
*--------------------------------------------------------------*
 320-CALC-MOD10         SECTION.
     MOVE     0              TO   WK-CD.
     MOVE     0              TO   J.
     COMPUTE  WK-KETA  =  10  -  LI-KETA.
     PERFORM  VARYING  I  FROM  WK-KETA  BY  1  UNTIL  I  >  8
              MOVE      WK-DENNO (I:1) TO   WK-CALC
              IF   J  =  0
                   ADD  WK-CALC        TO   WK-CD
                   MOVE      1         TO   J
              ELSE
                   COMPUTE  WK-CALC-2  =  WK-CALC  *  2
                   DIVIDE   WK-CALC-2  BY  10  GIVING    WK-DIV-1
                                               REMAINDER WK-DIV-2
                   COMPUTE  WK-CD = WK-CD + WK-DIV-1 + WK-DIV-2
                   MOVE      0         TO   J
              END-IF
     END-PERFORM.
*
     DIVIDE   WK-CD  BY  10  GIVING    WK-DIV-1
                             REMAINDER WK-DIV-2.
     COMPUTE  CALC-CD   =  10  -  WK-DIV-2.
 320-CALC-MOD10-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス１１の計算                        *
*--------------------------------------------------------------*
 330-CALC-MOD11         SECTION.
     MOVE     0              TO   WK-CD.
     COMPUTE  WK-KETA  =  10  -  LI-KETA.
     PERFORM  VARYING  I  FROM  WK-KETA  BY  1  UNTIL  I  >  8
              MOVE      WK-DENNO (I:1) TO   WK-CALC
              COMPUTE  WK-CD  =  WK-CD  +
                              +  (WK-CALC  *  (10 - I))
     END-PERFORM.
*
     DIVIDE   WK-CD  BY  11  GIVING    WK-DIV-1
                             REMAINDER WK-DIV-2.
     COMPUTE  CALC-CD   =  11  -  WK-DIV-2.
 330-CALC-MOD11-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス１１の計算                        *
*--------------------------------------------------------------*
 330-CALC-MOD11P2       SECTION.
     MOVE     0              TO   WK-CD.
     MOVE     0              TO   J.
     COMPUTE  WK-KETA  =  10  -  LI-KETA.
     PERFORM  VARYING  I  FROM  WK-KETA  BY  1  UNTIL  I  >  8
              MOVE      WK-DENNO (I:1) TO   WK-CALC
              IF   J  =  0
                   ADD  WK-CALC        TO   WK-CD
                   MOVE      1         TO   J
              ELSE
                   COMPUTE  WK-CALC-2  =  WK-CALC  *  3
                   COMPUTE  WK-CD = WK-CD + WK-CALC-2
                   MOVE      0         TO   J
              END-IF
     END-PERFORM.
*
     DIVIDE   WK-CD  BY  11  GIVING    WK-DIV-1
                             REMAINDER WK-DIV-2.
     COMPUTE  CALC-CD   =  11  -  WK-DIV-2.
 330-CALC-MOD11P2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス１１（ウエイト１・３）の計算      *
*--------------------------------------------------------------*
 330-CALC-MOD11P3       SECTION.
*
     MOVE     0              TO   WK-CD.
     MOVE     0              TO   J.
     COMPUTE  WK-KETA  =  10  -  LI-KETA.
     PERFORM  VARYING  I  FROM  WK-KETA  BY  1  UNTIL  I  >  8
              MOVE      WK-DENNO (I:1) TO   WK-CALC
              IF   J  =  1
                   COMPUTE  WK-CALC-2  =  WK-CALC  *  3
                   COMPUTE  WK-CD = WK-CD + WK-CALC-2
                   MOVE      0         TO   J
              ELSE
                   ADD  WK-CALC        TO   WK-CD
                   MOVE      1         TO   J
              END-IF
     END-PERFORM.
*
     DIVIDE   WK-CD  BY  10  GIVING    WK-DIV-1
                             REMAINDER WK-DIV-2.
     COMPUTE  CALC-CD   =  10  -  WK-DIV-2.
*
     IF       WK-DIV-2  =  ZERO
              MOVE         ZERO  TO  CALC-CD
     END-IF.
*
 330-CALC-MOD11P3-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス１０（ウエイト１・３）の計算      *
*--------------------------------------------------------------*
 320-CALC-MOD10P3       SECTION.
     MOVE     0              TO   WK-CD.
     MOVE     0              TO   J.
     COMPUTE  WK-KETA  =  10  -  LI-KETA.
     PERFORM  VARYING  I  FROM  WK-KETA  BY  1  UNTIL  I  >  8
              MOVE      WK-DENNO (I:1) TO   WK-CALC
              IF   J  =  0
                   ADD  WK-CALC        TO   WK-CD
                   MOVE      1         TO   J
              ELSE
                   COMPUTE  WK-CALC-2  =  WK-CALC  *  3
                   COMPUTE  WK-CD = WK-CD + WK-CALC-2
                   MOVE      0         TO   J
              END-IF
     END-PERFORM.
*
     DIVIDE   WK-CD  BY  10  GIVING    WK-DIV-1
                             REMAINDER WK-DIV-2.
     COMPUTE  CALC-CD   =  10  -  WK-DIV-2.
 320-CALC-MOD10P3-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      モジュラス１０（ウエイト３）の計算　　　    *
*--------------------------------------------------------------*
 320-CALC-MOD10W3       SECTION.
     MOVE     0              TO   WK-CD.
     MOVE     0              TO   J.
*偶数／奇数判定
     COMPUTE  WK-KETAW  =  LI-KETA  -  1.
     DIVIDE   WK-KETAW  BY  2  GIVING    WK-DIVW3-1
                               REMAINDER WK-DIVW3-2
*****偶数以外の場合
     IF  WK-DIVW3-2  NOT =  0
         MOVE 1              TO   J
     END-IF.
*
     COMPUTE  WK-KETA  =  10  -  LI-KETA.
     PERFORM  VARYING  I  FROM  WK-KETA  BY  1  UNTIL  I  >  8
              MOVE      WK-DENNO (I:1) TO   WK-CALC
              IF   J  =  0
                   ADD  WK-CALC        TO   WK-CD
                   MOVE      1         TO   J
              ELSE
                   COMPUTE  WK-CALC-2  =  WK-CALC  *  3
                   COMPUTE  WK-CD = WK-CD + WK-CALC-2
                   MOVE      0         TO   J
              END-IF
     END-PERFORM.
*
     DIVIDE   WK-CD  BY  10  GIVING    WK-DIV-1
                             REMAINDER WK-DIV-2.
     COMPUTE  CALC-CD   =  10  -  WK-DIV-2.
 320-CALC-MOD10W3-EXIT.
     EXIT.
