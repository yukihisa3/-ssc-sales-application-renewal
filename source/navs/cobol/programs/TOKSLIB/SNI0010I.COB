****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　売上データ抽出（条件入力）　　　　*
*    作成日／更新日　　　：　93/05/14 (SSKT160ﾖﾘ)              *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面より日次更新の条件を入力し　　*
*                        ：　条件ＦのＫＥＹ＝９９＋ＤＡＹに　　*
*                        ：　更新する　　　　　　　　　　　　　*
*    新基幹システム用へ変更:1999/10/16 T.TAKAHASHI             *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SNI0010I.
 AUTHOR.                T.A.
 DATE-WRITTEN.          93/05/14.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         HJYOKEN-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FNI00101    OF        XMDLIB.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
 01  INDEXES.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HJYOKEN-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-NOUHIN          PIC  9(08).
 01  FILLER             REDEFINES      WK-NOUHIN.
     03  WK-YYYY        PIC  9(02).
     03  WK-YMD         PIC  9(06).
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  CYU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      CYU-DATE.
     03  CYU-YY         PIC  9(04).
     03  CYU-MM         PIC  9(02).
     03  CYU-DD         PIC  9(02).
 01  NOU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      NOU-DATE.
     03  NOU-YY         PIC  9(04).
     03  NOU-MM         PIC  9(02).
     03  NOU-DD         PIC  9(02).
 01  SYU-DATE           PIC  9(08).
 01  FILLER             REDEFINES      SYU-DATE.
     03  SYU-YY         PIC  9(04).
     03  SYU-MM         PIC  9(02).
     03  SYU-DD         PIC  9(02).
 01  CNV-YY             PIC  9(04)     VALUE  0.
 01  ACOS-MM            PIC  9(02)     VALUE  0.
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF09           PIC  X(04)     VALUE     "F009".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_再入力".
*
 01  WK-JYO-F05    PIC  9(06).
**
*--<< ｻﾌﾞﾙｰﾁﾝ ﾊﾟﾗﾒﾀ >>-*
 01  SSKTDTCK-PARA.
     03  SSKTDTCK-YMD        PIC  9(06).
     03  FILLER              REDEFINES SSKTDTCK-YMD.
         05  SSKTDTCK-Y      PIC  9(02).
         05  SSKTDTCK-M      PIC  9(02).
         05  SSKTDTCK-D      PIC  9(02).
     03  SSKTDTCK-RET        PIC  9(01).
*
 01  WK-CHU-DT.
     03  WK-CHU-YY           PIC  9(02).
     03  WK-CHU-YMD.
         05  WK-CHU-Y        PIC  9(02).
         05  WK-CHU-M        PIC  9(02).
         05  WK-CHU-D        PIC  9(02).
*
 01  WK-NOU-DT.
     03  WK-NOU-YY           PIC  9(02).
     03  WK-NOU-YMD.
         05  WK-NOU-Y        PIC  9(02).
         05  WK-NOU-M        PIC  9(02).
         05  WK-NOU-D        PIC  9(02).
 01  WK-NOU-DTR      REDEFINES    WK-NOU-DT.
     03  WK-NOU-YMR          PIC  9(06).
     03  WK-NOU-DDR          PIC  9(02).
*
 01  WK-SYU-DT.
     03  WK-SYU-YY           PIC  9(02).
     03  WK-SYU-YMD.
         05  WK-SYU-Y        PIC  9(02).
         05  WK-SYU-M        PIC  9(02).
         05  WK-SYU-D        PIC  9(02).
*
 01  WK-SYS-DATE.
     03  WK-SYS-DATE-YY      PIC  9(02).
     03  WK-SYS-DATE-YMD.
         05  WK-SYS-DATE-Y   PIC  9(02).
         05  WK-SYU-DATE-M   PIC  9(02).
         05  WK-SYU-DATE-D   PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNI0010I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN  HTOKMS  DSPFILE.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNI0010I HJYOKEN ERROR " HJYOKEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN  HTOKMS  DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SNI0010I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HJYOKEN  HTOKMS  DSPFILE.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO     =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
*    入力日付をシステム日付と比較する為 1999/08/06 NAV
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-SYS-DATE.
*****MOVE     SYS-DATE  TO   WK-SYS-DATE-YMD.
*****IF       WK-SYS-DATE-Y  >  89
*****         MOVE  19  TO   WK-SYS-DATE-YY
*****ELSE
*****         MOVE  20  TO   WK-SYS-DATE-YY
*****END-IF.
**********************************************************
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SNI0010I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     I-O       HJYOKEN.
     OPEN     INPUT     HTOKMS.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
*----<<西暦変換>>-*
     INITIALIZE         FLAGS.
     MOVE     0         TO   GR-NO.
*****MOVE     57        TO   JYO-F01.
*****MOVE     SPACE     TO   JYO-F02.
*****PERFORM  900-JYO-READ.
*****IF       INV-FLG   =    0
*****         MOVE      JYO-F04   TO   CNV-YY
*****         REWRITE   JYO-REC
*****END-IF.
*****MOVE     SYS-DATE  TO   SYS-YYMD.
*****COMPUTE  SYS-YYYY  =    SYS-YY    +    CNV-YY.
     MOVE     WK-SYS-DATE  TO  SYS-YYMD.
*----<<経理月>>-*
     MOVE     58        TO   JYO-F01.
     MOVE     SPACE     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF      INV-FLG         =    1
     OR      ERR-FLG    NOT  =    0
             MOVE       99        TO   GR-NO
     END-IF.
     IF      INV-FLG         =    0
              REWRITE   JYO-REC
     END-IF.
*94/06/18 START *
*----<<在庫締>>-*
     MOVE     99        TO   JYO-F01.
     MOVE     "ZAI"     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     MOVE     JYO-F05   TO   WK-JYO-F05.
*
     DISPLAY  "在庫締日付＝" WK-JYO-F05  UPON CONS.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*取引先入力
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*発注日入力
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*納品日入力
     PERFORM  220-INP-GRP03  UNTIL     GR-NO    NOT  =    3.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*出荷日入力
     PERFORM  220-INP-GRP04  UNTIL     GR-NO    NOT  =    4.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*確認入力
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ｺｳｼﾝ   >>-*条件ファイル更新
     PERFORM  240-UPDT       UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    DSPFILE.
     CLOSE    HJYOKEN.
     CLOSE    HTOKMS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SNI0010I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE     SPACE          TO   FNI00101.
     MOVE     SYS-DATE       TO   DSPYMD.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FNI00101"       TO   DSP-FMT.
     MOVE     "SCREFX"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ（得意先コード入力）         *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "R00001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      "4010"    TO   PROGRAM-STATUS
         WHEN ENT
              IF   R00001    IS   NUMERIC
              AND  R00001    NOT  =    0
                   MOVE      R00001    TO   TOK-F01
                   PERFORM   900-TOK-READ
                   MOVE      TOK-F03   TO   R00011
                   IF   INV-FLG   =    0
                        ADD  1         TO   GR-NO
                   ELSE
                        MOVE NC"取引先が未登録です"   TO   MSG
                   END-IF
              ELSE
                   MOVE SPACE     TO   R00011
                   ADD  1         TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ（発注日入力）               *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "GRP02"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      "4010"    TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF09
              MOVE      1         TO   GR-NO
         WHEN ENT
              IF   (    R00002    IS   NOT  NUMERIC
                   AND  R00003    IS   NOT  NUMERIC
                   AND  R00004    IS   NOT  NUMERIC   )
              OR   (    R00002    =    0
                   AND  R00003    =    0
                   AND  R00004    =    0    )
                   MOVE ZERO      TO   CYU-DATE
                   ADD  1         TO   GR-NO
              ELSE
                   IF   R00002    IS   NOT  NUMERIC
                        MOVE      ZERO      TO   R00002
                   END-IF
                   IF   R00003    IS   NOT  NUMERIC
                        MOVE      ZERO      TO   R00003
                   END-IF
                   IF   R00004    IS   NOT  NUMERIC
                        MOVE      ZERO      TO   R00004
                   END-IF
                   MOVE R00002         TO   SSKTDTCK-Y
                   MOVE R00003         TO   SSKTDTCK-M
                   MOVE R00004         TO   SSKTDTCK-D
                   MOVE     "3"                 TO   LINK-IN-KBN
                   MOVE     SSKTDTCK-YMD        TO   LINK-IN-YMD6
                   MOVE     ZERO                TO   LINK-IN-YMD8
                   MOVE     ZERO                TO   LINK-OUT-RET
                   MOVE     ZERO                TO   LINK-OUT-YMD
                   CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                                     LINK-IN-YMD6
                                                     LINK-IN-YMD8
                                                     LINK-OUT-RET
                                                     LINK-OUT-YMD
                   IF   LINK-OUT-RET   =    0
                        MOVE LINK-OUT-YMD   TO   CYU-DATE
                        ADD  1              TO   GR-NO
                        IF   CYU-DATE  >  WK-SYS-DATE
                             MOVE NC"本日以降の日付入力不可"
                                                 TO  MSG
                             SUBTRACT   1   FROM GR-NO
                        END-IF
                   ELSE
                             MOVE NC"日付の指定が違います"
                                            TO   MSG
                   END-IF
              END-IF
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ（納品日入力）               *
*--------------------------------------------------------------*
 220-INP-GRP03          SECTION.
     MOVE     "GRP03"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      "4010"    TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF09
              MOVE      1         TO   GR-NO
         WHEN ENT
              IF   R00005    IS   NOT  NUMERIC
                   MOVE      ZERO      TO   R00005
              END-IF
              IF   R00006    IS   NOT  NUMERIC
                   MOVE      ZERO      TO   R00006
              END-IF
              IF   R00007    IS   NOT  NUMERIC
                   MOVE      ZERO      TO   R00007
              END-IF
              MOVE R00005         TO   SSKTDTCK-Y
              MOVE R00006         TO   SSKTDTCK-M
              MOVE R00007         TO   SSKTDTCK-D
              MOVE     "3"                 TO   LINK-IN-KBN
              MOVE     SSKTDTCK-YMD        TO   LINK-IN-YMD6
              MOVE     ZERO                TO   LINK-IN-YMD8
              MOVE     ZERO                TO   LINK-OUT-RET
              MOVE     ZERO                TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                                LINK-IN-YMD6
                                                LINK-IN-YMD8
                                                LINK-OUT-RET
                                                LINK-OUT-YMD
              IF   LINK-OUT-RET   =    0
                   MOVE LINK-OUT-YMD   TO   NOU-DATE
                   ADD  1              TO   GR-NO
                   IF   NOU-DATE(1:6)  <=  WK-JYO-F05
                        MOVE
                        NC"過去日付は入力できません" TO MSG

                        SUBTRACT   1   FROM GR-NO
                   ELSE
                        IF   NOU-DATE  >  WK-SYS-DATE
                             MOVE NC"本日以降の日付入力不可"
                                                 TO  MSG
                             SUBTRACT   1   FROM GR-NO
                        END-IF
                   END-IF
              ELSE
                        MOVE NC"日付の指定が違います"
                                        TO   MSG
              END-IF
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
 220-INP-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ（注文日入力）               *
*--------------------------------------------------------------*
 220-INP-GRP04          SECTION.
     MOVE     "GRP04"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      "4010"    TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF09
              MOVE      1         TO   GR-NO
         WHEN ENT
              IF   (    R00008    IS   NOT  NUMERIC
                   AND  R00009    IS   NOT  NUMERIC
                   AND  R00010    IS   NOT  NUMERIC   )
              OR   (    R00008    =    0
                   AND  R00009    =    0
                   AND  R00010    =    0    )
                   MOVE ZERO      TO   SYU-DATE
                   MOVE 9         TO   GR-NO
              ELSE
                   IF   R00008    IS   NOT  NUMERIC
                        MOVE      ZERO      TO   R00008
                   END-IF
                   IF   R00009    IS   NOT  NUMERIC
                        MOVE      ZERO      TO   R00009
                   END-IF
                   IF   R00010    IS   NOT  NUMERIC
                        MOVE      ZERO      TO   R00010
                   END-IF
                   MOVE R00008         TO   SSKTDTCK-Y
                   MOVE R00009         TO   SSKTDTCK-M
                   MOVE R00010         TO   SSKTDTCK-D
                   MOVE     "3"                 TO   LINK-IN-KBN
                   MOVE     SSKTDTCK-YMD        TO   LINK-IN-YMD6
                   MOVE     ZERO                TO   LINK-IN-YMD8
                   MOVE     ZERO                TO   LINK-OUT-RET
                   MOVE     ZERO                TO   LINK-OUT-YMD
                   CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                                     LINK-IN-YMD6
                                                     LINK-IN-YMD8
                                                     LINK-OUT-RET
                                                     LINK-OUT-YMD
                   IF   LINK-OUT-RET   =    0
                        MOVE LINK-OUT-YMD   TO   SYU-DATE
                        ADD  1              TO   GR-NO
**********************IF  NOU-DATE  NOT =  SYU-DATE
***********************MOVE NC"出荷日は納品日と同一にして下さい"
****************************************TO             MSG
**********************ELSE
                         IF   SYU-DATE   >  WK-SYS-DATE
                              MOVE NC"本日以降の日付入力不可"
                                                    TO  MSG
                         ELSE
                              MOVE 9         TO   GR-NO
                         END-IF
**********************END-IF
                   ELSE
                             MOVE NC"日付の指定が違います"
                                            TO   MSG
                   END-IF
              END-IF
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
 220-INP-GRP04-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      "4010"    TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF09
              MOVE      1         TO   GR-NO
         WHEN ENT
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSG
     END-EVALUATE.
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      条件Ｆ　更新　                              *
*--------------------------------------------------------------*
 240-UPDT               SECTION.
     MOVE     99        TO   JYO-F01.
     MOVE     "DAY"     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF      INV-FLG         =    0
        MOVE     R00001         TO   JYO-F04
        MOVE     CYU-DATE       TO   JYO-F05
        MOVE     NOU-DATE       TO   JYO-F06
        MOVE     SYU-DATE       TO   JYO-F07
        REWRITE  JYO-REC
     END-IF.
*
     MOVE     99             TO   GR-NO.
 240-UPDT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "SCRERE"       TO   DSP-GRP.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     PERFORM  900-DSP-WRITE.
*
     IF       MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSG.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FNI00101.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    条件ファイル　 READ                          *
*--------------------------------------------------------------*
 900-JYO-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID
              MOVE      1              TO   INV-FLG
     DISPLAY  "### SNI0010I HJYOKEN INVALID KEY= "
                                  JYO-F01   JYO-F02 " ###"
                                       UPON CONS
     END-READ.
 900-JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HTOKMS    INVALID
              MOVE      1         TO   INV-FLG
              MOVE      SPACE     TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
