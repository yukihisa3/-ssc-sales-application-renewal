****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫実績＆販売実績　　　　　　　　*
*    モジュール名　　　　：　振替実績　ＰＣ連携用Ｆ作成　　　　*
*    作成日／更新日　　　：　09/10/07                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　振替データを読み、ＰＣ連携用Ｆを　*
*                            作成する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU0500B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<< 電算室振替データ更新ファイル       >>---*
     SELECT   FURIKAF   ASSIGN    TO        DA-01-VI-FURIKAL3
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   FUR-F29
                                                 FUR-F01
                                                 FUR-F02
                                                 FUR-F03
                                                 FUR-F04
                                                 FUR-F05
                        FILE    STATUS      IS   FUR-STATUS.
*
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*---<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*---<< 取引先マスタ >>-*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS3
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F52
                        FILE      STATUS    IS   TOK-STATUS.
*---<< 商品変換テーブル >>-*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL4
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F031
                                                 TBL-F0321
                                                 TBL-F0322
                                                 TBL-F0323
                        FILE      STATUS    IS   TBL-STATUS.
*---<< 実績連携ファイル（オフコン累積）>>-*
     SELECT   PCJISSF   ASSIGN    TO        DA-01-VI-PCJISSL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   PCJ-F01
                                                 PCJ-F02
                                                 PCJ-F03
                                                 PCJ-F04
                                                 PCJ-F23
                                                 PCJ-F22
                                                 PCJ-F06
                                                 PCJ-F11
                                                 PCJ-F07
                                                 PCJ-F08
                        FILE      STATUS    IS   PCJ-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＴＯＫＵファイル  >>---*
 FD    FURIKAF.
       COPY   FURIKAF   OF        XFDLIB
              JOINING   FUR       PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  商品名称マスタ >>-*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  取引先マスタ >>-*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  商品変換テーブル >>-*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*---<< 実績連携ファイル（オフコン累積）>>-*
 FD  PCJISSF.
     COPY     PCJISSF   OF        XFDLIB
              JOINING   PCJ       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  FUR-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  TBL-STATUS          PIC  X(02).
     02  PCJ-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  PCJISSF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HSHOTBL-INV-FLG     PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     02  RD-CNT              PIC  9(07)  VALUE ZERO.
     02  WT-CNT              PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT1           PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT2           PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT3           PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT4           PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT5           PIC  9(07)  VALUE ZERO.
     02  WK-JYO-F04          PIC  9(09)  VALUE ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SFU0500B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
 01  SYS-TIME2               PIC  9(08).
 01  FILLER            REDEFINES  SYS-TIME2.
     03  SYS-TIMEW           PIC  9(06).
     03  FILLER              PIC  9(02).
 01  WK-SYS-TIME             PIC  9(08)  VALUE  ZERO.
*実績連携ファイルワーク
     COPY   PCJISSF  OF XFDLIB  JOINING   PWK  AS   PREFIX.
****  ＷＲＫ領域  ***
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(06).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(04).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
     02  SYS-DATE            PIC  9(06)  VALUE  ZERO.
     02  SYS-DATE-YMD        PIC  9(08)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    FURIKAF.
     MOVE     "FURIKAF"         TO   ERR-FL-ID.
     MOVE     FUR-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "JYOKEN1"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "MEIMS1 "      TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTOKMS.
     MOVE    "TOKMS3  "      TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBL.
     MOVE    "SHOTBL4 "      TO   ERR-FL-ID.
     MOVE     TBL-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC6                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    PCJISSF.
     MOVE    "PCJISSL1"      TO   ERR-FL-ID.
     MOVE     PCJ-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 CONTROL-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 CONTROL-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN     INPUT          FURIKAF  HMEIMS  HTOKMS  HSHOTBL
              I-O            PCJISSF  HJYOKEN.
*システム日付の取得
     ACCEPT   SYS-DATE          FROM   DATE.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE-YMD.
     ACCEPT    SYS-TIME           FROM TIME.
     MOVE      SYS-TIME           TO   SYS-TIME2.
*
     PERFORM    READ-FURIKAF-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*実績連携ファイル初期化
     MOVE   SPACE            TO   PWK-REC.
     INITIALIZE                   PWK-REC.
*存在チェックの実施
     EVALUATE  FUR-F02
         WHEN  "70"  WHEN  "71"
               MOVE FUR-F01       TO   PWK-F01
               MOVE ZERO          TO   PWK-F02
               MOVE ZERO          TO   PWK-F03
               MOVE FUR-F02       TO   PWK-F04
               MOVE SPACE         TO   PWK-F05
               IF   FUR-F02 = "70"
                    IF   FUR-F19 = "1" OR "2"
                         MOVE "I3"     TO   PWK-F05
                    END-IF
               END-IF
               MOVE FUR-F05       TO   PWK-F06
               MOVE FUR-F03       TO   PWK-F07
               MOVE FUR-F04       TO   PWK-F08
               MOVE FUR-F07       TO   PWK-F09
               MOVE FUR-F08       TO   PWK-F10
               MOVE FUR-F09       TO   PWK-F11
               MOVE SPACE         TO   PWK-F12
               MOVE FUR-F10       TO   PWK-F13
               MOVE FUR-F11(1:5)  TO   PWK-F14
               MOVE FUR-F11(6:2)  TO   PWK-F15
               MOVE FUR-F11(8:1)  TO   PWK-F16
               MOVE FUR-F13       TO   PWK-F17
               MOVE FUR-F13       TO   PWK-F18
               MOVE FUR-F14       TO   PWK-F19
               MOVE ZERO          TO   PWK-F20
               MOVE ZERO          TO   PWK-F21
               MOVE FUR-F16       TO   PWK-F22
               MOVE FUR-F17       TO   PWK-F23
               MOVE MEI-F90       TO   PWK-F24
               PERFORM HJYOKEN-READ-SEC
               MOVE WK-JYO-F04    TO   PWK-FIL(1:9)
         WHEN  "40"  WHEN  "41"
               MOVE FUR-F01       TO   PWK-F01
***************MOVE FUR-F24       TO   PWK-F02
*取引先ＣＤ変換
               PERFORM  HTOKMS-READ-SEC
               IF   HTOKMS-INV-FLG = SPACE
                    MOVE  TOK-F01 TO   PWK-F02
               ELSE
********************MOVE  ZERO    TO   PWK-F02
                    GO            TO   MAIN010
               END-IF
               MOVE FUR-F28       TO   PWK-F03
               MOVE FUR-F02       TO   PWK-F04
               MOVE SPACE         TO   PWK-F05
               MOVE FUR-F05       TO   PWK-F06
               MOVE FUR-F03       TO   PWK-F07
               MOVE FUR-F04       TO   PWK-F08
               MOVE FUR-F07       TO   PWK-F09
               MOVE FUR-F08       TO   PWK-F10
               MOVE FUR-F09       TO   PWK-F11
               MOVE SPACE         TO   PWK-F12
               MOVE FUR-F10       TO   PWK-F13
               MOVE FUR-F11(1:5)  TO   PWK-F14
               MOVE FUR-F11(6:2)  TO   PWK-F15
               MOVE FUR-F11(8:1)  TO   PWK-F16
               MOVE FUR-F13       TO   PWK-F17
               MOVE FUR-F13       TO   PWK-F18
               MOVE FUR-F14       TO   PWK-F19
***************2010/01/21 原価単価を変換ＴＢＬより取得
***************MOVE FUR-F25       TO   PWK-F20
               PERFORM HSHOTBL-READ-SEC
               IF   HSHOTBL-INV-FLG = SPACE
               AND  TBL-F05 > ZERO
                    MOVE  TBL-F05 TO  PWK-F20
               ELSE
                    PERFORM HMEIMS-READ-SEC
                    IF  HMEIMS-INV-FLG = SPACE
                        MOVE MEI-F042  TO  PWK-F20
**** 決まるまでコメント IF   MEI-F042  =  ZERO
*                            GO             TO  MAIN010
****                    END-IF
                    ELSE
************************原価単価が取得できない場合、対象外
************************MOVE ZERO      TO  PWK-F20
                        GO             TO  MAIN010
                    END-IF
               END-IF
********************
               MOVE ZERO          TO   PWK-F21
               MOVE FUR-F16       TO   PWK-F22
               MOVE FUR-F17       TO   PWK-F23
               MOVE MEI-F90       TO   PWK-F24
               PERFORM HJYOKEN-READ-SEC
               MOVE WK-JYO-F04    TO   PWK-FIL(1:9)
         WHEN  "50"  WHEN  "51"
               MOVE FUR-F01       TO   PWK-F01
               MOVE ZERO          TO   PWK-F02
               MOVE ZERO          TO   PWK-F03
               MOVE FUR-F02       TO   PWK-F04
               MOVE SPACE         TO   PWK-F05
               MOVE FUR-F05       TO   PWK-F06
               MOVE FUR-F03       TO   PWK-F07
               MOVE FUR-F04       TO   PWK-F08
               MOVE FUR-F07       TO   PWK-F09
               MOVE FUR-F08       TO   PWK-F10
               MOVE FUR-F09       TO   PWK-F11
               MOVE SPACE         TO   PWK-F12
               MOVE FUR-F10       TO   PWK-F13
               MOVE FUR-F11(1:5)  TO   PWK-F14
               MOVE FUR-F11(6:2)  TO   PWK-F15
               MOVE FUR-F11(8:1)  TO   PWK-F16
               MOVE FUR-F13       TO   PWK-F17
               MOVE FUR-F13       TO   PWK-F18
               MOVE FUR-F14       TO   PWK-F19
               MOVE ZERO          TO   PWK-F20
               MOVE ZERO          TO   PWK-F21
               MOVE FUR-F16       TO   PWK-F22
               MOVE FUR-F17       TO   PWK-F23
               MOVE MEI-F90       TO   PWK-F24
               PERFORM HJYOKEN-READ-SEC
               MOVE WK-JYO-F04    TO   PWK-FIL(1:9)
*********WHEN  "31"  WHEN  "32"
         WHEN  "32"
               MOVE FUR-F01       TO   PWK-F01
               MOVE ZERO          TO   PWK-F02
               MOVE ZERO          TO   PWK-F03
               MOVE FUR-F02       TO   PWK-F04
               MOVE "I3"          TO   PWK-F05
               MOVE FUR-F05       TO   PWK-F06
               MOVE FUR-F03       TO   PWK-F07
               MOVE FUR-F04       TO   PWK-F08
               MOVE FUR-F09       TO   PWK-F09
               MOVE FUR-F09       TO   PWK-F10
               MOVE FUR-F09       TO   PWK-F11
               MOVE SPACE         TO   PWK-F12
               MOVE FUR-F10       TO   PWK-F13
               MOVE FUR-F11(1:5)  TO   PWK-F14
               MOVE FUR-F11(6:2)  TO   PWK-F15
               MOVE FUR-F11(8:1)  TO   PWK-F16
               MOVE FUR-F13       TO   PWK-F17
               MOVE FUR-F13       TO   PWK-F18
               MOVE ZERO          TO   PWK-F19
               MOVE ZERO          TO   PWK-F20
               MOVE ZERO          TO   PWK-F21
               MOVE FUR-F16       TO   PWK-F22
               MOVE FUR-F17       TO   PWK-F23
               MOVE MEI-F90       TO   PWK-F24
               PERFORM HJYOKEN-READ-SEC
               MOVE WK-JYO-F04    TO   PWK-FIL(1:9)
*********WHEN  "35"  WHEN  "36"
*              MOVE FUR-F01       TO   PWK-F01
*              MOVE ZERO          TO   PWK-F02
*              MOVE ZERO          TO   PWK-F03
*              MOVE FUR-F02       TO   PWK-F04
*              MOVE SPACE         TO   PWK-F05
*              MOVE FUR-F05       TO   PWK-F06
*              MOVE FUR-F03       TO   PWK-F07
*              MOVE FUR-F04       TO   PWK-F08
*              MOVE FUR-F07       TO   PWK-F09
*              MOVE FUR-F08       TO   PWK-F10
*              MOVE FUR-F09       TO   PWK-F11
*              MOVE SPACE         TO   PWK-F12
*              MOVE FUR-F10       TO   PWK-F13
*              MOVE FUR-F11(1:5)  TO   PWK-F14
*              MOVE FUR-F11(6:2)  TO   PWK-F15
*              MOVE FUR-F11(8:1)  TO   PWK-F16
*              MOVE FUR-F13       TO   PWK-F17
*              MOVE FUR-F13       TO   PWK-F18
*              MOVE ZERO          TO   PWK-F19
*              MOVE ZERO          TO   PWK-F20
*              MOVE ZERO          TO   PWK-F21
*              MOVE FUR-F16       TO   PWK-F22
*              MOVE FUR-F17       TO   PWK-F23
*              MOVE MEI-F90       TO   PWK-F24
*              PERFORM HJYOKEN-READ-SEC
***************MOVE WK-JYO-F04    TO   PWK-FIL(1:9)
         WHEN  OTHER
               GO                 TO   MAIN010
     END-EVALUATE.
*存在チェック
     MOVE      SPACE         TO   PCJ-REC.
     INITIALIZE                   PCJ-REC.
     MOVE      PWK-REC       TO   PCJ-REC.
     PERFORM   PCJISSF-READ-SEC.
     IF  PCJISSF-INV-FLG = SPACE
         ADD     1           TO   SKIP-CNT1
         GO                  TO   MAIN010
     END-IF.
*データ作成日セット
     MOVE      SYS-DATE-YMD  TO   PCJ-F98.
     MOVE      SYS-TIME2     TO   PCJ-F99.
*レコード出力
     WRITE     PCJ-REC.
     ADD       1             TO   WT-CNT.
*
 MAIN010.
     PERFORM    READ-FURIKAF-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE   FURIKAF  HJYOKEN  HTOKMS  HMEIMS PCJISSF HSHOTBL.
     DISPLAY  "FURIKAF    (IN) = "  RD-CNT        UPON   CONS.
     DISPLAY  "ｼﾞｯｾｷﾚﾝｹｲF(ADD) = "  WT-CNT        UPON   CONS.
     DISPLAY  "SKIPｶｳﾝﾀｰ-ｼﾞｭｳﾌｸ= "  SKIP-CNT1     UPON   CONS.
     DISPLAY  "SKIPｶｳﾝﾀｰ-ﾀｲｼｮｳK= "  SKIP-CNT2     UPON   CONS.
     DISPLAY  "SKIPｶｳﾝﾀｰ-ﾒｲｼｮｳX= "  SKIP-CNT3     UPON   CONS.
     DISPLAY  "SKIPｶｳﾝﾀｰ-ｶﾝﾘKBN= "  SKIP-CNT4     UPON   CONS.
 END-END.
     EXIT.
****************************************************************
*      1.1 振替データ読込　　　　　　　　　　　　　　　　　　　*
****************************************************************
 READ-FURIKAF-SEC               SECTION.
     READ    FURIKAF
         AT   END
           MOVE   "END"      TO   END-FLG
           GO                TO   READ-FURIKAF-END
         NOT  AT  END
           ADD     1         TO   RD-CNT
     END-READ.
 RD01.
*****DISPLAY "DEN-NO = " FUR-F03.
*管理区分が１の時は対象外
     IF    FUR-F29 = "1"
           MOVE   "END"      TO   END-FLG
           GO                TO   READ-FURIKAF-END
     END-IF.
 RD02.
*更新区分が１の時は対象外
*****DISPLAY "FUR-F19 = "  FUR-F19.
     IF    FUR-F19 = "1"  OR  "2"  OR  "3"  OR  "4"
           CONTINUE
     ELSE
           ADD     1         TO   SKIP-CNT2
           GO TO                  READ-FURIKAF-SEC
     END-IF.
 RD03.
*商品ＣＤで商品名称マスタを索引
     PERFORM HMEIMS-READ-SEC.
     IF    HMEIMS-INV-FLG = "INV"
           ADD     1         TO   SKIP-CNT3
           GO                TO   READ-FURIKAF-SEC
     ELSE
*管理区分チェック
           IF       MEI-F91  =  SPACE
                    ADD   1  TO   SKIP-CNT4
                    GO       TO   READ-FURIKAF-SEC
           END-IF
     END-IF.
*
 READ-FURIKAF-END.
     EXIT.
****************************************************************
*      ALL        商品名称マスタ読込　　　　                   *
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     MOVE     FUR-F10   TO    MEI-F011.
     MOVE     FUR-F11   TO    MEI-F012.
     MOVE     SPACE     TO    HMEIMS-INV-FLG.
     READ     HMEIMS
              INVALID     MOVE  "INV"    TO  HMEIMS-INV-FLG
                          GO             TO  HMEIMS-READ-EXIT
              NOT INVALID MOVE  SPACE    TO  HMEIMS-INV-FLG
     END-READ.
 MEI01.
*管理区分チェック
*****IF       MEI-F91  =  SPACE
*             MOVE "INV"  TO                 HMEIMS-INV-FLG
*****END-IF.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*      ALL        振替実績Ｆ読込　　　　　　                   *
****************************************************************
 PCJISSF-READ-SEC       SECTION.
*
     READ     PCJISSF
              INVALID     MOVE  "INV"    TO  PCJISSF-INV-FLG
                          GO             TO  PCJISSF-READ-EXIT
              NOT INVALID MOVE  SPACE    TO  PCJISSF-INV-FLG
     END-READ.
*
 PCJISSF-READ-EXIT.
     EXIT.
****************************************************************
*      ALL        条件ファイル読込　　　　　                   *
****************************************************************
 HJYOKEN-READ-SEC       SECTION.
*
     MOVE     "99"        TO             JYO-F01.
     MOVE     "PCSVR"     TO             JYO-F02.
     READ     HJYOKEN
              INVALID
              DISPLAY NC"＃＃送信用伝票番号　採番エラー＃＃"
                      UPON CONS
              MOVE    "4000"      TO     PROGRAM-STATUS
              STOP  RUN
     END-READ.
*
     ADD      1                   TO     JYO-F04.
     MOVE     JYO-F04             TO     WK-JYO-F04.
*
     IF       JYO-F04  >=  JYO-F06
     OR       JYO-F04  =   ZERO
              MOVE   JYO-F05      TO     JYO-F04
              MOVE   JYO-F04      TO     WK-JYO-F04
     END-IF.
*
     REWRITE  JYO-REC.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*      ALL        取引先マスタ読込　　　　　                   *
****************************************************************
 HTOKMS-READ-SEC        SECTION.
*
     MOVE     FUR-F06   TO    TOK-F52.
     MOVE     SPACE     TO    HTOKMS-INV-FLG.
     READ     HTOKMS
              INVALID     MOVE  "INV"    TO  HTOKMS-INV-FLG
              NOT INVALID MOVE  SPACE    TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*      ALL        商品変換テーブル　　　　　                   *
****************************************************************
 HSHOTBL-READ-SEC       SECTION.
*
     MOVE     PWK-F02   TO    TBL-F01.
     MOVE     FUR-F10   TO    TBL-F031.
     MOVE     FUR-F11   TO    TBL-F032.
     MOVE     SPACE     TO    HSHOTBL-INV-FLG.
     READ     HSHOTBL
              INVALID     MOVE  "INV"    TO  HSHOTBL-INV-FLG
              NOT INVALID MOVE  SPACE    TO  HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************
