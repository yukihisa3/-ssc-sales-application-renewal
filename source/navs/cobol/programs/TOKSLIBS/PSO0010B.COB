****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　新規取引先オンライン（ダイユー）　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　受信データ編集（ＰＯＳ）　　　　　*
*    作成日／更新日　　　：　10/07/12                          *
*    作成者／更新者　　　：　ＮＡＶ　阿部　　　　　　　　　　　*
*    処理概要　　　　　　：　ＰＯＳ受信データを読み、　　　　　*
*                            ＰＯＳ売上情報ファイルを出力する　*
*    作成日／更新日　　　：　13/09/20                          *
*                            日敷追加　　　　　　　　　　　　　*
*    作成日／更新日　　　：　2018/01/29                        *
*                            リック資材／植物追加　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            PSO0010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/07/12.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   CVCPS001  ASSIGN    TO        DA-01-S-CVCPS001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    POS-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＰＯＳ売上情報ファイル
     SELECT   DYPSURF   ASSIGN    TO        DA-01-VI-DYPSURL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       PSJ-F01   PSJ-F02
                                            PSJ-F03   PSJ-F04
                                            PSJ-F05
                        FILE  STATUS   IS   PSJ-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　ＢＦ＝　１
******************************************************************
 FD  CVCPS001
                        BLOCK CONTAINS      8    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  POS-REC.
     03  POS-01.
         05  POS-01A             PIC  X(02).
         05  POS-01B             PIC  X(464).
******************************************************************
*    ＰＯＳ売上情報ファイル
******************************************************************
 FD  DYPSURF            LABEL RECORD   IS   STANDARD.
     COPY     DYPSURF   OF        XFDLIB
              JOINING   PSJ  AS   PREFIX.
******************************************************************
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*発注データ（ヘッダ）
     COPY     DYPSHDF  OF   XFDLIB   JOINING   PSH    PREFIX.
*発注データ（明細１）
     COPY     DYPSME1  OF   XFDLIB   JOINING   PM1    PREFIX.
*発注データ（明細２）
     COPY     DYPSME2  OF   XFDLIB   JOINING   PM2    PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-JANCD.
     03  WK-JANCD1         PIC X(13).
     03  FILLER            PIC X(01).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  POS-STATUS        PIC  X(02).
     03  PSJ-STATUS        PIC  X(02).
*
*特売売上数量、売上数量変換領域
 01  WK-SURYO-X.
     03  FILLER            PIC  X(01).
     03  WK-SURYO-X1       PIC  9(07).
     03  FILLER            PIC  X(01).
     03  WK-SURYO-X2       PIC  9(01).
     03  FILLER            PIC  X(01).
 01  WK-SURYO-R.
     03  WK-SURYOR1        PIC  9(07).
     03  WK-SURYOR2        PIC  9(01).
 01  WK-SURYOR             REDEFINES WK-SURYO-R.
     03  WK-SURYO          PIC  9(08).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "PSO0010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "PSO0010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "PSO0010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE                          DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVCPS001.
     MOVE      "CVCPS001"   TO   AB-FILE.
     MOVE      POS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYPSURF.
     MOVE      "DYPSURF"    TO   AB-FILE.
     MOVE      PSJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     INITIALIZE                   WK-AREA.
     OPEN     INPUT     CVCPS001
     OPEN     OUTPUT    DYPSURF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*    MOVE     SPACE     TO        WK-FLHD-REC.
*    INITIALIZE                   WK-FLHD-REC.
     MOVE     SPACE     TO        PSH-REC.
     INITIALIZE                   PSH-REC.
     MOVE     SPACE     TO        PM1-REC.
     INITIALIZE                   PM1-REC.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     READ     CVCPS001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
        EVALUATE  POS-01A
           WHEN   "HD"
*                 伝票ヘッダ処理
                  MOVE      SPACE     TO    PSH-REC
                  MOVE      SPACE     TO    PSJ-REC
                  INITIALIZE                PSH-REC
                  INITIALIZE                PSJ-REC
                  MOVE      POS-01    TO    PSH-REC
                  PERFORM   DYHED-TENSO-SEC
           WHEN   "P1"
*                 明細情報１
                  MOVE      SPACE     TO    PM1-REC
                  INITIALIZE                PM1-REC
                  MOVE      POS-01    TO    PM1-REC
                  PERFORM   DYPS1-TENSO-SEC
           WHEN   "P2"
*                 明細情報２
                  MOVE      SPACE     TO    PM2-REC
                  INITIALIZE                PM2-REC
                  MOVE      POS-01    TO    PM2-REC
                  PERFORM   DYPS2-TENSO-SEC
        END-EVALUATE.
*
     READ     CVCPS001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　受信ヘッダ転送                                    *
****************************************************************
 DYHED-TENSO-SEC             SECTION.
*
     MOVE    "DYHED-TENSO-SEC"   TO        S-NAME.
     MOVE     SPACE         TO        PSJ-REC.
     INITIALIZE                       PSJ-REC.
*取引先コード
*    2013/09/20 桁数間違い？　↓
*----EVALUATE    PSH-H05(1:5)
     EVALUATE    PSH-H05(1:4)
*    2013/09/20 桁数間違い？　↑
         WHEN    "4950"
                 MOVE       PSH-H03     TO      PSJ-F01
*##2010/11/25 ﾀﾞｲﾕｰｴｲﾄMAXは、ﾀﾞｲﾕｰｴｲﾄに含める
         WHEN    "4959"
                 MOVE       "001225"    TO      PSJ-F01
         WHEN    "4952"
                 MOVE       "012251"    TO      PSJ-F01
*       2013/09/20 日敷　追加 ↓
         WHEN    "4953"
                 MOVE       "012252"    TO      PSJ-F01
*       2013/09/20 日敷　追加 ↑
*       2018/01/29 リック追加 ↓
         WHEN    "4978"
                 MOVE       PSH-H03     TO      PSJ-F01
*       2018/01/29 リック追加 ↑
     END-EVALUATE.
*受信日
     MOVE        SYS-DATEW   TO      PSJ-F03.
*仕入先ＴＥＬ
     MOVE        PSH-H00     TO      PSJ-H00.
 DYHED-TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　受信明細１転送                                    *
****************************************************************
 DYPS1-TENSO-SEC             SECTION.
*
     MOVE    "DYPS1-TENSO-SEC"   TO        S-NAME.
*
*店舗コード
     MOVE    PM1-M102      TO        PSJ-F02.
*売上年月日
     MOVE    PM1-M103      TO        PSJ-F05.
*ＰＯＳ売上情報明細１
     MOVE    PM1-REC       TO        PSJ-M100.
*
 DYPS1-TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　受信明細２転送　　　　　                          *
****************************************************************
 DYPS2-TENSO-SEC             SECTION.
*              R
     MOVE    "DYPS2-TENSO-SEC"   TO        S-NAME.
*標準商品コード
*****MOVE     PM2-M203     TO        WK-JANCD.
*****MOVE     WK-JANCD1    TO        PSJ-F04.
     MOVE     PM2-M203     TO        PSJ-F04.
*ＰＯＳ売上情報明細２
     MOVE     PM2-REC      TO        PSJ-M200.
*特売売上数量、データ変換
     INITIALIZE     WK-SURYO-X  WK-SURYO-R.
*
     MOVE     PM2-M212     TO        WK-SURYO-X.
     MOVE     WK-SURYO-X1  TO        WK-SURYOR1.
     MOVE     WK-SURYO-X2  TO        WK-SURYOR2.
     MOVE     WK-SURYO     TO        PSJ-M212.
*売上数量データ変換
     INITIALIZE     WK-SURYO-X  WK-SURYO-R.
*
     MOVE     PM2-M213     TO        WK-SURYO-X.
     MOVE     WK-SURYO-X1  TO        WK-SURYOR1.
     MOVE     WK-SURYO-X2  TO        WK-SURYOR2.
     MOVE     WK-SURYO     TO        PSJ-M213.
*
     WRITE   PSJ-REC
     ADD     1             TO        WRT-CNT.
 DYPS2-TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     CVCPS001  DYPSURF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
