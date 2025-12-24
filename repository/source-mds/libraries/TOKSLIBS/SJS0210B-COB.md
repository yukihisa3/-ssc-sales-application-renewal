# SJS0210B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJS0210B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫実績＆販売実績　　　　　　　　*
*    モジュール名　　　　：　各実績データ累積処理（日次系）　　*
*    作成日／更新日　　　：　09/10/13                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　日次計上Ｆを順読みし、連携用Ｆを　*
*                            作成する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJS0210B.
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
*---<< 日次系　計上ファイル　>>---*
     SELECT   TOKU      ASSIGN    TO        DA-01-S-TOKU
                        FILE    STATUS      IS   TOKU-STATUS.
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
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
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
 FD    TOKU   BLOCK     CONTAINS  4    RECORDS.
       COPY   TOKUREC   OF        XFDLIB
              JOINING   TOKU      PREFIX.
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
*---<< 実績連携ファイル（オフコン累積）>>-*
 FD  PCJISSF.
     COPY     PCJISSF   OF        XFDLIB
              JOINING   PCJ       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  TOKU-STATUS         PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  PCJ-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  PCJISSF-INV-FLG     PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     02  RD-CNT              PIC  9(07)  VALUE ZERO.
     02  WT-CNT              PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT1           PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT2           PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT3           PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT4           PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT5           PIC  9(07)  VALUE ZERO.
     02  SKIP-CNT6           PIC  9(07)  VALUE ZERO.
     02  WK-TOKCD            PIC  9(08)  VALUE ZERO.
     02  WK-JYO-F04          PIC  9(09)  VALUE ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SJS0210B".
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
                     PROCEDURE    TOKU.
     MOVE     "TOKU    "     TO   ERR-FL-ID.
     MOVE     TOKU-STATUS    TO   ERR-STCD.
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
     MOVE    "TOKMS1  "      TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5                SECTION.
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
     OPEN     INPUT          TOKU  HMEIMS  HTOKMS
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
     PERFORM    READ-TOKU-SEC.
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
     EVALUATE  TOKU-F02
         WHEN  40  WHEN  41  WHEN  42  WHEN  47
               MOVE TOKU-F01      TO   PWK-F01
               MOVE TOKU-F39      TO   PWK-F02
               MOVE TOKU-F07      TO   PWK-F03
               MOVE TOKU-F02      TO   PWK-F04
               IF   TOKU-F02 =  42
               AND  TOKU-F40 = "47"
                    MOVE TOKU-F40 TO  PWK-F04
                    MOVE 47       TO  TOKU-F02
               END-IF
               MOVE SPACE         TO   PWK-F05
               MOVE TOKU-F05      TO   PWK-F06
               MOVE TOKU-F03      TO   PWK-F07
               MOVE TOKU-F04      TO   PWK-F08
               MOVE TOKU-F08      TO   PWK-F09
               MOVE TOKU-F09      TO   PWK-F10
               MOVE TOKU-F10      TO   PWK-F11
               MOVE TOKU-F24      TO   PWK-F12
               MOVE TOKU-F11      TO   PWK-F13
               MOVE TOKU-F12(1:5) TO   PWK-F14
               MOVE TOKU-F12(6:2) TO   PWK-F15
               MOVE TOKU-F12(8:1) TO   PWK-F16
               MOVE TOKU-F41      TO   PWK-F17
               MOVE TOKU-F15      TO   PWK-F18
               MOVE TOKU-F37      TO   PWK-F19
               MOVE TOKU-F16      TO   PWK-F20
               MOVE TOKU-F38      TO   PWK-F21
               MOVE TOKU-F19      TO   PWK-F22
               MOVE TOKU-F18      TO   PWK-F23
               MOVE MEI-F90       TO   PWK-F24
***************IF   TOKU-F02 = 41
               IF   TOKU-F02 = 41  OR  47
                    IF  TOKU-F05 = 0  OR  2  OR 4
                        MOVE   1      TO   PWK-F22
                    ELSE
                        MOVE   2      TO   PWK-F22
                    END-IF
               END-IF
***************IF   TOKU-F02 = 40 OR  42 OR 47
               IF   TOKU-F02 = 40 OR  42
                    IF  TOKU-F05 = 0  OR  2  OR 4
                        MOVE   2      TO   PWK-F22
                    ELSE
                        MOVE   1      TO   PWK-F22
                    END-IF
               END-IF
               PERFORM HJYOKEN-READ-SEC
               MOVE WK-JYO-F04    TO   PWK-FIL(1:9)
         WHEN  50  WHEN  51
               MOVE TOKU-F01      TO   PWK-F01
               MOVE TOKU-F06      TO   PWK-F02
               MOVE TOKU-F07      TO   PWK-F03
               MOVE TOKU-F02      TO   PWK-F04
               MOVE SPACE         TO   PWK-F05
               MOVE ZERO          TO   PWK-F06
               MOVE TOKU-F03      TO   PWK-F07
               MOVE TOKU-F04      TO   PWK-F08
               MOVE TOKU-F08      TO   PWK-F09
               MOVE TOKU-F09      TO   PWK-F10
               MOVE TOKU-F10      TO   PWK-F11
               MOVE SPACE         TO   PWK-F12
               MOVE TOKU-F11      TO   PWK-F13
               MOVE TOKU-F12(1:5) TO   PWK-F14
               MOVE TOKU-F12(6:2) TO   PWK-F15
               MOVE TOKU-F12(8:1) TO   PWK-F16
               MOVE TOKU-F15      TO   PWK-F17
               MOVE TOKU-F15      TO   PWK-F18
               MOVE TOKU-F16      TO   PWK-F19
               MOVE ZERO          TO   PWK-F20
               MOVE ZERO          TO   PWK-F21
***************MOVE TOKU-F19      TO   PWK-F22
               IF   TOKU-F02 = 50
                    MOVE   "1"    TO   PWK-F22
               ELSE
                    MOVE   "2"    TO   PWK-F22
               END-IF
               MOVE TOKU-F18      TO   PWK-F23
               MOVE MEI-F90       TO   PWK-F24
               PERFORM HJYOKEN-READ-SEC
               MOVE WK-JYO-F04    TO   PWK-FIL(1:9)
         WHEN  31  WHEN  32
               MOVE TOKU-F01      TO   PWK-F01
               MOVE ZERO          TO   PWK-F02
               MOVE ZERO          TO   PWK-F03
               MOVE "32"          TO   PWK-F04
               MOVE SPACE         TO   PWK-F05
               MOVE ZERO          TO   PWK-F06
               MOVE TOKU-F03      TO   PWK-F07
               MOVE TOKU-F04      TO   PWK-F08
               EVALUATE TOKU-F04 ALSO TOKU-F19
                   WHEN 1        ALSO 1
                        MOVE 1         TO   PWK-F08
                   WHEN 1        ALSO 2
                        MOVE 2         TO   PWK-F08
                   WHEN 2        ALSO 1
                        MOVE 3         TO   PWK-F08
                   WHEN 2        ALSO 2
                        MOVE 4         TO   PWK-F08
                   WHEN 3        ALSO 1
                        MOVE 5         TO   PWK-F08
                   WHEN 3        ALSO 2
                        MOVE 6         TO   PWK-F08
                   WHEN 4        ALSO 1
                        MOVE 7         TO   PWK-F08
                   WHEN 4        ALSO 2
                        MOVE 8         TO   PWK-F08
                   WHEN 5        ALSO 1
                        MOVE 9         TO   PWK-F08
                   WHEN 5        ALSO 2
                        MOVE 10        TO   PWK-F08
                   WHEN 6        ALSO 1
                        MOVE 11        TO   PWK-F08
                   WHEN 6        ALSO 2
                        MOVE 12        TO   PWK-F08
               END-EVALUATE
               MOVE TOKU-F08      TO   PWK-F09
               MOVE TOKU-F09      TO   PWK-F10
               MOVE TOKU-F10      TO   PWK-F11
               MOVE SPACE         TO   PWK-F12
               MOVE TOKU-F11      TO   PWK-F13
               MOVE TOKU-F12(1:5) TO   PWK-F14
               MOVE TOKU-F12(6:2) TO   PWK-F15
               MOVE TOKU-F12(8:1) TO   PWK-F16
               MOVE TOKU-F15      TO   PWK-F17
               MOVE TOKU-F15      TO   PWK-F18
               MOVE TOKU-F16      TO   PWK-F19
               MOVE ZERO          TO   PWK-F20
               MOVE ZERO          TO   PWK-F21
               MOVE TOKU-F19      TO   PWK-F22
               MOVE TOKU-F18      TO   PWK-F23
               MOVE MEI-F90       TO   PWK-F24
               PERFORM HJYOKEN-READ-SEC
               MOVE WK-JYO-F04    TO   PWK-FIL(1:9)
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
     PERFORM    READ-TOKU-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              TOKU  HJYOKEN  HTOKMS  HMEIMS PCJISSF.
     DISPLAY  "TOKU    (IN)    = "  RD-CNT        UPON   CONS.
     DISPLAY  "ｼﾞｯｾｷﾚﾝｹｲF(ADD) = "  WT-CNT        UPON   CONS.
     DISPLAY  "SKIPｶｳﾝﾀｰ-ｼﾞｭｳﾌｸ= "  SKIP-CNT1     UPON   CONS.
     DISPLAY  "SKIPｶｳﾝﾀｰ-ｸﾌﾞﾝNG= "  SKIP-CNT2     UPON   CONS.
     DISPLAY  "SKIPｶｳﾝﾀｰ-I3    = "  SKIP-CNT3     UPON   CONS.
     DISPLAY  "SKIPｶｳﾝﾀｰ-ﾄﾘﾋｷｻｷ= "  SKIP-CNT4     UPON   CONS.
     DISPLAY  "SKIPｶｳﾝﾀｰ-ｶﾝﾘKBN= "  SKIP-CNT5     UPON   CONS.
     DISPLAY  "SKIPｶｳﾝﾀｰ-ｹｯﾋﾟﾝK= "  SKIP-CNT6     UPON   CONS.
 END-END.
     EXIT.
****************************************************************
*      1.1 振替データ読込　　　　　　　　　　　　　　　　　　　*
****************************************************************
 READ-TOKU-SEC               SECTION.
     READ    TOKU
         AT   END
           MOVE   "END"      TO   END-FLG
           GO                TO   READ-TOKU-END
         NOT  AT  END
           ADD     1         TO   RD-CNT
     END-READ.
*
     IF    RD-CNT(5:3) = "000" OR "500"
           DISPLAY "RD-CNT = " RD-CNT UPON CONS
     END-IF.
*抽出対象の伝票区分判定
 RD01.
     IF    TOKU-F02 = 40 OR 41 OR 42 OR 47 OR
           50 OR 51 OR 31 OR 32
           CONTINUE
     ELSE
           ADD     1         TO   SKIP-CNT2
           GO TO                  READ-TOKU-SEC
     END-IF.
*伝票区分＝３１，３２の時の判定
 RD02.
     IF    TOKU-F02 = 31 OR 32
           IF   TOKU-F21  NOT =  "I3"
                ADD     1         TO   SKIP-CNT3
                GO TO                  READ-TOKU-SEC
           END-IF
     END-IF.
*取引先抽出チェック
 RD03.
*    取引先ＣＤより索引
     IF      TOKU-F02 = 40 OR 41 OR 42 OR 47
             MOVE    TOKU-F39        TO   WK-TOKCD
             PERFORM HTOKMS-READ-SEC
             IF    HTOKMS-INV-FLG = "INV"
                   ADD     1         TO   SKIP-CNT4
                   GO TO                  READ-TOKU-SEC
             END-IF
     END-IF.
 RD04.
*商品ＣＤで商品名称マスタを索引
     PERFORM HMEIMS-READ-SEC.
     IF    HMEIMS-INV-FLG = "INV"
           ADD     1         TO   SKIP-CNT5
           GO TO                  READ-TOKU-SEC
     END-IF.
 RD05.
*欠品制御区分判定
     IF    TOKU-F42  =  "1"
           ADD     1         TO   SKIP-CNT6
           GO TO                  READ-TOKU-SEC
     END-IF.
*
 READ-TOKU-END.
     EXIT.
****************************************************************
*      ALL        商品名称マスタ読込　　　　                   *
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     MOVE     TOKU-F11   TO    MEI-F011.
     MOVE     TOKU-F12   TO    MEI-F012.
     MOVE     SPACE     TO    HMEIMS-INV-FLG.
     READ     HMEIMS
              INVALID     MOVE  "INV"    TO  HMEIMS-INV-FLG
                          GO             TO  HMEIMS-READ-EXIT
              NOT INVALID MOVE  SPACE    TO  HMEIMS-INV-FLG
     END-READ.
 MEI31.
*管理区分チェック
     IF       MEI-F91  =  SPACE
              MOVE "INV"  TO                 HMEIMS-INV-FLG
     END-IF.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*      ALL        取引先マスタ読込　　　　　                   *
****************************************************************
 HTOKMS-READ-SEC        SECTION.
*
     MOVE     WK-TOKCD  TO    TOK-F01.
     MOVE     SPACE     TO    HTOKMS-INV-FLG.
     READ     HTOKMS
              INVALID     MOVE  "INV"    TO  HTOKMS-INV-FLG
                          GO             TO  HTOKMS-READ-EXIT
              NOT INVALID MOVE  SPACE    TO  HTOKMS-INV-FLG
     END-READ.
 TOK01.
*管理区分チェック
     IF       TOK-F78  =  SPACE
              MOVE "INV"  TO                 HTOKMS-INV-FLG
     END-IF.
*
 HTOKMS-READ-EXIT.
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
******************<<  PROGRAM  END  >>**************************

```
