****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　            *
*    モジュール名　　　　：　在庫ＥＸＣＥＬ取込エラーリスト    *
*    作成日／作成者　　　：　2016/06/14 INOUE                  *
*    処理概要　　　　　　：　取込チェックにてエラーとなった　　*
*                            データをリスト出力する。　　　　　*
*    更新日／更新者　　　：　2016/07/11 INOUE                  *
*                            作業区分(G1/G5)対応
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SZI0050L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/06/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YA-22     IS   YA-22
     YB-22     IS   YB-22
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*在庫EXCEL取込ファイル
     SELECT   ZAIWKXX1  ASSIGN    TO        DA-01-VI-ZAIWKXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       EXL-F060
                                            EXL-F051
                        FILE  STATUS   IS   EXL-STATUS.
*担当者マスタ
     SELECT     TANMS1       ASSIGN    TO       DA-01-VI-TANMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01  TAN-F02
                             FILE      STATUS   TAN-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*    在庫EXCEL取込ファイル
 FD  ZAIWKXX1            LABEL RECORD   IS   STANDARD.
     COPY     ZAIWKXX1   OF        XFDLIB
              JOINING   EXL       PREFIX.
*担当者マスタ
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  ZAIWKXX1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  ZERO.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*ステータス
 01  WK-ST.
     03  KAK-STATUS        PIC  X(02).
     03  EXL-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SAK-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SZI0050L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SZI0050L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SZI0050L".
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
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER  TYPE YB-22.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "SZI0050L".
         05  FILLER          PIC  X(27)     VALUE  SPACE.
         05  FILLER          PIC  N(17)     VALUE
         NC"＜在庫ＥＸＣＥＬ取込エラーリスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(19)     VALUE  SPACE.
         05  HD00-YYYY       PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD00-MM         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD00-DD         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD00-HH         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD00-SS         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD00-MS         PIC  9(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD00-PCNT       PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD000.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(07)
                             VALUE  NC"取込担当者　：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD000-TRBUMON   PIC  X(04).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  HD000-TRTANTO   PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD000-TRTANNM   PIC  N(10).
*
 01  HD01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"エラー情報→".
         05  FILLER          PIC  N(06)     VALUE
             NC"１作業区分　".
         05  FILLER          PIC  N(06)     VALUE
             NC"２倉庫ＣＤ　".
         05  FILLER          PIC  N(06)     VALUE
             NC"３必須項目　".
         05  FILLER          PIC  N(06)     VALUE
             NC"４商品ＣＤ　".
         05  FILLER          PIC  N(05)     VALUE
             NC"５数量　　".
         05  FILLER          PIC  N(05)     VALUE
             NC"６作業日　".
         05  FILLER          PIC  N(06)     VALUE
             NC"７部門ＣＤ　".
         05  FILLER          PIC  N(07)     VALUE
             NC"８入出庫区分　".
         05  FILLER          PIC  N(06)     VALUE
             NC"９２整合性".
         05  FILLER          PIC  N(05)     VALUE
             NC"０行番号".
*
 01  HD02.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(114)    VALUE  SPACE.
         05  FILLER          PIC  N(10)
                             VALUE NC"＜　エラー情報　　＞".
 01  HD04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"連番".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"作業区分".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"部門".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"倉→倉".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"作業日".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(08)
                                  VALUE NC"サカタ商品コード".
         05  FILLER          PIC  X(05)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(04)     VALUE  NC"出庫棚番".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"入庫棚番".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"入出".
         05  FILLER          PIC  X(05)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  N(02)     VALUE  NC"数量".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"未".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  X(04)     VALUE  "ｽﾄNO".
         05  FILLER          PIC  N(01)     VALUE  NC"→".
         05  FILLER          PIC  X(04)     VALUE  "ｽﾄNO".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"備考".
         05  FILLER          PIC  X(07)     VALUE  SPACE.
         05  FILLER          PIC  N(11)
                             VALUE NC"１２３４５６７８９０".
*
 01  MS01.
     03  FILLER              CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-RENBAN     PIC  X(05).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SKUBUN     PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SKUBUNNM   PIC  N(04).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BUMON      PIC  X(04).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SOUKOF     PIC  X(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SOUKOT     PIC  X(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SAGYOUBI   PIC  X(05).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SYOCD      PIC  X(19).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-TANAS      PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TANAN      PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  MS01-NSK        PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SUURYOU    PIC  Z,ZZZ,ZZ9.
         05  MS01-SUURYOUX   REDEFINES  MS01-SUURYOU.
             07 MS01-SURYOUX PIC  X(09).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-MISYUKKO   PIC  N(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-STOCKF     PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-STOCKT     PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BIKOU      PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  MS01-ERR1       PIC  N(01).
         05  MS01-ERR2       PIC  N(01).
         05  MS01-ERR3       PIC  N(01).
         05  MS01-ERR4       PIC  N(01).
         05  MS01-ERR5       PIC  N(01).
         05  MS01-ERR6       PIC  N(01).
         05  MS01-ERR7       PIC  N(01).
         05  MS01-ERR8       PIC  N(01).
         05  MS01-ERR9       PIC  N(01).
         05  MS01-ERR10      PIC  N(01).
*
*対象データなし
 01  LST-DATA-X.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃".
 01  LST-DATA-Y.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　取込データにエラーはありません。！！　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".
*時刻編集
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BUMON         PIC   X(04).
 01  PARA-IN-TANCD         PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-BUMON
                                       PARA-IN-TANCD.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZAIWKXX1.
     MOVE      "ZAIWKXX1"   TO   AB-FILE.
     MOVE      EXL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TANMS1.
     MOVE      "TANMS1  "   TO   AB-FILE.
     MOVE      TAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF    "   TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     ZAIWKXX1
                        TANMS1.
     OPEN     OUTPUT    PRTF.
*
     DISPLAY  MSG-START UPON CONS.
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
     ACCEPT   SYS-TIME          FROM   TIME.
*在庫EXCEL取込ファイルスタート
     PERFORM  ZAIWKXX1-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　エラーデータ　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*在庫EXCEL取込ファイル読込
     PERFORM ZAIWKXX1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　エラーデータ　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    在庫EXCEL取込ファイルスタート
****************************************************************
 ZAIWKXX1-START-SEC          SECTION.
*
     MOVE    "ZAIWKXX1-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   EXL-REC.
     INITIALIZE                        EXL-REC.
*
     MOVE     "1"                 TO   EXL-F060.
     MOVE     ZERO                TO   EXL-F051.
*
     START  ZAIWKXX1  KEY  IS  >=      EXL-F060 EXL-F051
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 ZAIWKXX1-START-EXIT.
     EXIT.
*
****************************************************************
*    在庫EXCEL取込ファイル読込
****************************************************************
 ZAIWKXX1-READ-SEC           SECTION.
*
     MOVE    "ZAIWKXX1-READ-SEC"   TO   S-NAME.
*
     READ     ZAIWKXX1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   ZAIWKXX1-READ-EXIT
     END-READ.
*エラー区分チェック
     IF       EXL-F060  NOT =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   ZAIWKXX1-READ-EXIT
     END-IF.
*件数カウント
     ADD      1                   TO   ZAIWKXX1-READ-CNT.
*
 ZAIWKXX1-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-010.
*印刷処理
     PERFORM  MEISAI-WT-SEC.
     MOVE     SPACE  TO   PRT-REC.
*
 MAIN-020.
     PERFORM  ZAIWKXX1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO                TO   LINE-CNT.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD00-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD00-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD00-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD00-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD00-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD00-MS.
*    担当者名取得
     MOVE     PARA-IN-BUMON       TO   TAN-F01 HD000-TRBUMON.
     MOVE     PARA-IN-TANCD       TO   TAN-F02 HD000-TRTANTO.
     PERFORM  TANMS1-READ-SEC.
     IF  TANMS1-INV-FLG = SPACE
               MOVE  TAN-F03      TO   HD000-TRTANNM
     ELSE
               MOVE  ALL NC"？"   TO   HD000-TRTANNM
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD000 AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     10                  TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*   改頁判定
     IF       LINE-CNT  >   50
              PERFORM HEAD-WT-SEC
     END-IF.
*   連番
     IF       EXL-F051  NOT NUMERIC
              MOVE  "?????"       TO   MS01-RENBAN
     ELSE
              MOVE  EXL-F051      TO   MS01-RENBAN
     END-IF.
*   作業区分
     MOVE     EXL-F052            TO   MS01-SKUBUN.
     EVALUATE EXL-F052
        WHEN  "I3"
              MOVE  NC"製品移動"  TO   MS01-SKUBUNNM
        WHEN  "46"
              MOVE  NC"廃棄　　"  TO   MS01-SKUBUNNM
        WHEN  "55"
              MOVE  NC"棚移動　"  TO   MS01-SKUBUNNM
        WHEN  "58"
              MOVE  NC"ストック"  TO   MS01-SKUBUNNM
        WHEN  "G1"
              MOVE  NC"セット組"  TO   MS01-SKUBUNNM
        WHEN  "G5"
              MOVE  NC"ばらし　"  TO   MS01-SKUBUNNM
        WHEN  OTHER
              MOVE  NC"？？？？"  TO   MS01-SKUBUNNM
     END-EVALUATE.
*   部門
     MOVE     EXL-F053            TO   MS01-BUMON.
*   倉庫（元）
     MOVE     EXL-F054            TO   MS01-SOUKOF.
*   倉庫（先）
     IF       EXL-F052   = "I3"
              MOVE    EXL-F055    TO   MS01-SOUKOT
     ELSE
              MOVE    SPACE       TO   MS01-SOUKOT
     END-IF.
*   作業日
     IF       EXL-F056  NOT NUMERIC
              MOVE  "?????"       TO   MS01-SAGYOUBI
     ELSE
              MOVE  EXL-F056(5:2) TO   MS01-SAGYOUBI(1:2)
              MOVE  "/"           TO   MS01-SAGYOUBI(3:1)
              MOVE  EXL-F056(7:2) TO   MS01-SAGYOUBI(4:2)
     END-IF.
*   サカタ商品コード
     MOVE     EXL-F057            TO   MS01-SYOCD(1:8).
     MOVE     "-"                 TO   MS01-SYOCD(9:1).
     MOVE     EXL-F058            TO   MS01-SYOCD(10:5).
     MOVE     "-"                 TO   MS01-SYOCD(15:1).
     MOVE     EXL-F059            TO   MS01-SYOCD(16:2).
     MOVE     "-"                 TO   MS01-SYOCD(18:1).
     MOVE     EXL-F05A            TO   MS01-SYOCD(19:1).
*   出庫棚番
     EVALUATE EXL-F052
        WHEN  "I3"
              MOVE  EXL-F05I      TO   MS01-TANAS
        WHEN  "46"
              MOVE  EXL-F05I      TO   MS01-TANAS
        WHEN  "55"
              MOVE  EXL-F05B      TO   MS01-TANAS
        WHEN  "58"
              MOVE  EXL-F05B      TO   MS01-TANAS
        WHEN  "G1"
              IF    EXL-F05P  NOT =    "1"
                    MOVE  EXL-F05Q      TO   MS01-TANAS
              ELSE
                    MOVE  SPACE         TO   MS01-TANAS
              END-IF
        WHEN  "G5"
              IF    EXL-F05P  NOT =    "1"
                    MOVE  EXL-F05Q      TO   MS01-TANAS
              ELSE
                    MOVE  SPACE         TO   MS01-TANAS
              END-IF
        WHEN  OTHER
              MOVE  "??????"      TO   MS01-TANAS
     END-EVALUATE.
*   入庫棚番
     EVALUATE EXL-F052
        WHEN  "I3"
              MOVE  EXL-F05J      TO   MS01-TANAN
        WHEN  "46"
              MOVE  EXL-F05J      TO   MS01-TANAN
        WHEN  "55"
              MOVE  EXL-F05C      TO   MS01-TANAN
        WHEN  "58"
              MOVE  EXL-F05C      TO   MS01-TANAN
        WHEN  "G1"
              IF    EXL-F05P  NOT =    "2"
                    MOVE  EXL-F05Q      TO   MS01-TANAN
              ELSE
                    MOVE  SPACE         TO   MS01-TANAN
              END-IF
        WHEN  "G5"
              IF    EXL-F05P  NOT =    "2"
                    MOVE  EXL-F05Q      TO   MS01-TANAN
              ELSE
                    MOVE  SPACE         TO   MS01-TANAN
              END-IF
        WHEN  OTHER
              MOVE  "??????"      TO   MS01-TANAN
     END-EVALUATE.
*   入出庫区分
     MOVE     NC"　"              TO   MS01-NSK.
     IF       EXL-F052    =  "G1" OR "G5"
              MOVE  NC"？"                TO   MS01-NSK
              IF    EXL-F05P  =   "1"
                    MOVE     NC"入"       TO   MS01-NSK
              END-IF
              IF    EXL-F05P  =   "2"
                    MOVE     NC"出"       TO   MS01-NSK
              END-IF
     END-IF.
*   数量
     MOVE     "?????????"         TO   MS01-SURYOUX.
     IF       EXL-F052    =  "I3"
              IF    EXL-F05L  NOT NUMERIC
                    MOVE    "?????????"   TO   MS01-SURYOUX
              ELSE
                    MOVE    EXL-F05L      TO   MS01-SUURYOU
              END-IF
     END-IF.
     IF       EXL-F052    =  "46"
              IF    EXL-F05L  NOT NUMERIC
                    MOVE    "?????????"   TO   MS01-SURYOUX
              ELSE
                    MOVE    EXL-F05L      TO   MS01-SUURYOU
              END-IF
     END-IF.
     IF       EXL-F052    =  "55"
              IF    EXL-F05F  NOT NUMERIC
                    MOVE    "?????????"   TO   MS01-SURYOUX
              ELSE
                    MOVE    EXL-F05F      TO   MS01-SUURYOU
              END-IF
     END-IF.
     IF       EXL-F052    =  "58"
              IF    EXL-F05F  NOT NUMERIC
                    MOVE    "?????????"   TO   MS01-SURYOUX
              ELSE
                    MOVE    EXL-F05F      TO   MS01-SUURYOU
              END-IF
     END-IF.
     IF       EXL-F052    =  "G1"
              IF    EXL-F05R  NOT NUMERIC
                    MOVE    "?????????"   TO   MS01-SURYOUX
              ELSE
                    MOVE    EXL-F05R      TO   MS01-SUURYOU
              END-IF
     END-IF.
     IF       EXL-F052    =  "G5"
              IF    EXL-F05R  NOT NUMERIC
                    MOVE    "?????????"   TO   MS01-SURYOUX
              ELSE
                    MOVE    EXL-F05R      TO   MS01-SUURYOU
              END-IF
     END-IF.
*   未出庫ＦＬＧ
     MOVE     NC"　"              TO   MS01-MISYUKKO.
     IF       EXL-F052   =  "55"
        MOVE  NC"？"              TO   MS01-MISYUKKO
        IF    EXL-F05H   =  "1"
              MOVE  NC"未"        TO   MS01-MISYUKKO
        END-IF
     END-IF.
*   ストック（元）
     EVALUATE EXL-F052
        WHEN  "I3"
              MOVE  EXL-F05K      TO   MS01-STOCKF
        WHEN  "46"
              MOVE  EXL-F05K      TO   MS01-STOCKF
        WHEN  "55"
              MOVE  SPACE         TO   MS01-STOCKF
        WHEN  "58"
              MOVE  EXL-F05D      TO   MS01-STOCKF
        WHEN  "G1"
              MOVE  SPACE         TO   MS01-STOCKF
        WHEN  "G5"
              MOVE  SPACE         TO   MS01-STOCKF
        WHEN  OTHER
              MOVE  "??????"      TO   MS01-STOCKF
     END-EVALUATE.
*   ストック（先）
     IF       EXL-F052   = "58"
              MOVE    EXL-F05E    TO   MS01-STOCKT
     ELSE
              MOVE    SPACE       TO   MS01-STOCKT
     END-IF.
*   備考
     EVALUATE EXL-F052
        WHEN  "I3"
              MOVE  EXL-F05M      TO   MS01-BIKOU
        WHEN  "46"
              MOVE  EXL-F05M      TO   MS01-BIKOU
        WHEN  "55"
              MOVE  EXL-F05G      TO   MS01-BIKOU
        WHEN  "58"
              MOVE  EXL-F05G      TO   MS01-BIKOU
        WHEN  "G1"
              MOVE  EXL-F05S      TO   MS01-BIKOU
        WHEN  "G5"
              MOVE  EXL-F05S      TO   MS01-BIKOU
        WHEN  OTHER
              MOVE  "??????????"  TO   MS01-BIKOU
     END-EVALUATE.
*   エラー区分１
     IF  EXL-F061 = " "
              MOVE SPACE          TO   MS01-ERR1
     ELSE
              MOVE NC"×"         TO   MS01-ERR1
     END-IF.
*    エラー区分２
     IF  EXL-F062 = " "
              MOVE SPACE          TO   MS01-ERR2
     ELSE
              MOVE NC"×"         TO   MS01-ERR2
     END-IF.
*    エラー区分３
     IF  EXL-F063 = " "
              MOVE SPACE          TO   MS01-ERR3
     ELSE
              MOVE NC"×"         TO   MS01-ERR3
     END-IF.
*    エラー区分４
     IF  EXL-F064 = " "
              MOVE SPACE          TO   MS01-ERR4
     ELSE
              MOVE NC"×"         TO   MS01-ERR4
     END-IF.
*    エラー区分５
     IF  EXL-F065 = " "
              MOVE SPACE          TO   MS01-ERR5
     ELSE
              MOVE NC"×"         TO   MS01-ERR5
     END-IF.
*    エラー区分６
     IF  EXL-F066 = " "
              MOVE SPACE          TO   MS01-ERR6
     ELSE
              MOVE NC"×"         TO   MS01-ERR6
     END-IF.
*    エラー区分７
     IF  EXL-F067 = " "
              MOVE SPACE          TO   MS01-ERR7
     ELSE
              MOVE NC"×"         TO   MS01-ERR7
     END-IF.
*    エラー区分８
     IF  EXL-F068 = " "
              MOVE SPACE          TO   MS01-ERR8
     ELSE
              MOVE NC"×"         TO   MS01-ERR8
     END-IF.
*    エラー区分９
     IF  EXL-F069 = " "
              MOVE SPACE          TO   MS01-ERR9
     ELSE
              MOVE NC"×"         TO   MS01-ERR9
     END-IF.
*    エラー区分１０
     IF  EXL-F06A = " "
              MOVE SPACE          TO   MS01-ERR10
     ELSE
              MOVE NC"×"         TO   MS01-ERR10
     END-IF.
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
*行カウント
     ADD      1                   TO    LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*在庫EXCEL取込ファイル読込
     DISPLAY "ZAIWKXX1 PRINT-CNT = " ZAIWKXX1-READ-CNT UPON CONS.
*
     CLOSE     ZAIWKXX1  TANMS1  PRTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TANMS1-READ-SEC           SECTION.
     MOVE "TANMS1-READ-SEC"       TO   S-NAME.
*
     READ       TANMS1
         INVALID       MOVE "INV" TO   TANMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
