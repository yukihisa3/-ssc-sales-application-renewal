****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　社内振替　　　　　　　            *
*    モジュール名　　　　：　社内振替発注ＥＸＣＥＬ　　　　　　*
*                                チェック結果リスト            *
*    作成日／作成者　　　：　2016/12/26 INOUE                  *
*    処理概要　　　　　　：　取込チェックにてエラーとなった　　*
*                            データをリスト出力する。　　　　　*
*    流用　　　　　　　　：　SDE0050L                          *
*    更新日／更新者　　　：　                                  *
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SFU3020L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/12/26.
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
*振替EXCEL取込ファイル
     SELECT   SFRWKL1   ASSIGN    TO        DA-01-VI-SFRWKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SFR-F051
                                            SFR-F052
                                            SFR-F053
                                            SFR-F054
                                            SFR-F055
                                            SFR-F056
                                            SFR-F057
                                            SFR-F058
                        FILE  STATUS   IS   SFR-STATUS.
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
*振替EXCEL取込ファイル
 FD  SFRWKL1            LABEL RECORD   IS   STANDARD.
     COPY     SFRWKL1   OF        XFDLIB
     JOINING  SFR       PREFIX.
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
 01  SFRWKL1-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  ZERO.
 01  WK-SUURYO               PIC S9(09)     VALUE  ZERO.
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
     03  SFR-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SAK-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SFU3020L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SFU3020L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SFU3020L".
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
 01  HD0.
     03  FILLER         CHARACTER  TYPE YB-22.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "SFU3020L".
         05  FILLER          PIC  X(15)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
         NC"＜社内振替発注ＥＸＣＥＬチェック結果リスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(16)     VALUE  SPACE.
         05  HD0-YYYY        PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD0-MM          PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD0-DD          PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD0-HH          PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD0-SS          PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD0-MS          PIC  9(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD0-PCNT        PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD00.
         05  FILLER          PIC  X(200)    VALUE  SPACE.
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
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"エラー情報→".
         05  FILLER          PIC  N(05)     VALUE
             NC"_必須項目".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_同一キー".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE
             NC"_伝区".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE
             NC"_年度".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_シーズン".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_倉庫Ｍ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_名称Ｍ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"_入荷予定日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_仕入先Ｍ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_完納済".
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_正常　".
*
 01  HD02.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"伝区".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"年度".
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"シーズン".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"倉庫".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  X(03)     VALUE  "(  ".
         05  FILLER          PIC  N(07)
                                  VALUE  NC"サカタ商品情報".
         05  FILLER          PIC  X(04)     VALUE  "   )".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)
                                  VALUE  NC"ＪＡＮＣＤ".
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"商品名".
         05  FILLER          PIC  X(40)     VALUE  SPACE.
         05  FILLER          PIC  N(12)     VALUE
                             NC"＜　　エラー情報　　　＞".
 01  HD04.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(06)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"入荷予定".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  N(03)     VALUE  NC"仕入先".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"棚番".
         05  FILLER          PIC  X(07)     VALUE  SPACE.
         05  FILLER          PIC  N(05)
                                  VALUE  NC"発　注　数".
         05  FILLER          PIC  X(22)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"ＭＳＧ".
         05  FILLER          PIC  X(39)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(10)
                             VALUE NC"__________".
         05  FILLER          PIC  N(05)
                             VALUE NC"_____".
*
 01  MS01.
     03  FILLER.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-DENKU      PIC  X(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-NENDO      PIC  9(04).
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  MS01-SEASON     PIC  X(02).
         05  FILLER          PIC  X(07)     VALUE  SPACE.
         05  MS01-SOUKO      PIC  X(02).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  MS01-SYOCD1     PIC  X(09).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SYOCD2     PIC  X(11).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-JANCD      PIC  X(13).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  MS01-SYONAME.
             07  MS01-SYONAME1    PIC  N(15).
             07  MS01-SYONAME2    PIC  N(15).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
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
         05  MS01-ERR11      PIC  N(01).
         05  MS01-ERR12      PIC  N(01).
         05  MS01-ERR13      PIC  N(01).
         05  MS01-ERR14      PIC  N(01).
         05  MS01-ERR15      PIC  N(01).
*
 01  MS02.
     03  FILLER.
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  MS02-YOTEI      PIC  9(08).
         05  MS02-YOTEX      REDEFINES  MS02-YOTEI.
             07 MS02-YOTEIX  PIC  X(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-SIIRESAKI  PIC  X(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-TANA       PIC  X(06).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS02-HACU       PIC  ----,---,--9.
         05  MS02-HACX       REDEFINES  MS02-HACU.
             07 MS02-HACUX   PIC  X(12).
         05  FILLER          PIC  X(21)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  MS02-MSG        PIC  N(30).
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
             NC"＃　　　　取込データが０件です　！！　　　＃".
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
 01  PARA-IN-JKUBUN        PIC   X(01).
 01  PARA-IN-BUMON         PIC   X(04).
 01  PARA-IN-TANCD         PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-JKUBUN
                                       PARA-IN-BUMON
                                       PARA-IN-TANCD.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRWKL1.
     MOVE      "SFRWKL1"    TO   AB-FILE.
     MOVE      SFR-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SFRWKL1
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
*振替EXCEL取込ファイルスタート
     PERFORM  SFRWKL1-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*振替EXCEL取込ファイル読込
     PERFORM SFRWKL1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし　＃＃"  UPON CONS
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
*    振替EXCEL取込ファイルスタート
****************************************************************
 SFRWKL1-START-SEC          SECTION.
*
     MOVE    "SFRWKL1-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   SFR-REC.
     INITIALIZE                        SFR-REC.
*
     MOVE     SPACE               TO   SFR-F051.
     MOVE     ZERO                TO   SFR-F052.
     MOVE     SPACE               TO   SFR-F053.
     MOVE     SPACE               TO   SFR-F054.
     MOVE     SPACE               TO   SFR-F055.
     MOVE     SPACE               TO   SFR-F056.
     MOVE     SPACE               TO   SFR-F057.
     MOVE     SPACE               TO   SFR-F058.
*
     START  SFRWKL1  KEY  IS  >=      SFR-F051 SFR-F052
                                      SFR-F053 SFR-F054
                                      SFR-F055 SFR-F056
                                      SFR-F057 SFR-F058
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 SFRWKL1-START-EXIT.
     EXIT.
*
****************************************************************
*    振替EXCEL取込ファイル読込
****************************************************************
 SFRWKL1-READ-SEC           SECTION.
*
     MOVE    "SFRWKL1-READ-SEC"   TO   S-NAME.
*
     READ     SFRWKL1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   SFRWKL1-READ-EXIT
     END-READ.
*エラー区分チェック
*    IF       SFR-F301  NOT =  "1"
*             MOVE     "END"      TO   END-FLG
*             GO                  TO   SFRWKL1-READ-EXIT
*    END-IF.
*件数カウント
     ADD      1                   TO   SFRWKL1-READ-CNT.
*
 SFRWKL1-READ-EXIT.
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
     PERFORM  SFRWKL1-READ-SEC.
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
     MOVE     PAGE-CNT            TO   HD0-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD0-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD0-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD0-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD0-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD0-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD0-MS.
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
     WRITE    PRT-REC       FROM  HD0   AFTER  2.
     WRITE    PRT-REC       FROM  HD00  AFTER  1.
     WRITE    PRT-REC       FROM  HD000 AFTER  1.
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
*
*明細１行目
*   伝区
     MOVE     SFR-F051            TO   MS01-DENKU.
*   年度
     IF       SFR-F052  NOT NUMERIC
              MOVE  "?????"       TO   MS01-NENDO
     ELSE
              MOVE  SFR-F052      TO   MS01-NENDO
     END-IF.
*   シーズン
     MOVE     SFR-F053            TO   MS01-SEASON.
*   倉庫
     MOVE     SFR-F054            TO   MS01-SOUKO.
*   サカタ商品情報
     MOVE     "("                 TO   MS01-SYOCD1(1:1).
     MOVE     SFR-F055            TO   MS01-SYOCD1(2:8).
     MOVE     SFR-F056            TO   MS01-SYOCD2(1:5).
     MOVE     "-"                 TO   MS01-SYOCD2(6:1).
     MOVE     SFR-F057            TO   MS01-SYOCD2(7:2).
     MOVE     "-"                 TO   MS01-SYOCD2(9:1).
     MOVE     SFR-F058            TO   MS01-SYOCD2(10:1).
     MOVE     ")"                 TO   MS01-SYOCD2(11:1).
*   ＪＡＮＣＤ
     MOVE     SFR-F059            TO   MS01-JANCD.
*   商品名
     MOVE     SFR-F05A            TO   MS01-SYONAME.
*
*   エラー区分１
     IF  SFR-F061 = " "
              MOVE SPACE          TO   MS01-ERR1
     ELSE
              MOVE NC"×"         TO   MS01-ERR1
     END-IF.
*    エラー区分２
     IF  SFR-F062 = " "
              MOVE SPACE          TO   MS01-ERR2
     ELSE
              MOVE NC"×"         TO   MS01-ERR2
     END-IF.
*    エラー区分３
     IF  SFR-F063 = " "
              MOVE SPACE          TO   MS01-ERR3
     ELSE
              MOVE NC"×"         TO   MS01-ERR3
     END-IF.
*    エラー区分４
     IF  SFR-F064 = " "
              MOVE SPACE          TO   MS01-ERR4
     ELSE
              MOVE NC"×"         TO   MS01-ERR4
     END-IF.
*    エラー区分５
     IF  SFR-F065 = " "
              MOVE SPACE          TO   MS01-ERR5
     ELSE
              MOVE NC"×"         TO   MS01-ERR5
     END-IF.
*    エラー区分６
     IF  SFR-F066 = " "
              MOVE SPACE          TO   MS01-ERR6
     ELSE
              MOVE NC"×"         TO   MS01-ERR6
     END-IF.
*    エラー区分７
     IF  SFR-F067 = " "
              MOVE SPACE          TO   MS01-ERR7
     ELSE
              MOVE NC"×"         TO   MS01-ERR7
     END-IF.
*    エラー区分８
     IF  SFR-F068 = " "
              MOVE SPACE          TO   MS01-ERR8
     ELSE
              MOVE NC"×"         TO   MS01-ERR8
     END-IF.
*    エラー区分９
     IF  SFR-F069 = " "
              MOVE SPACE          TO   MS01-ERR9
     ELSE
              MOVE NC"×"         TO   MS01-ERR9
     END-IF.
*    エラー区分１０
     IF  SFR-F06A = " "
              MOVE SPACE          TO   MS01-ERR10
     ELSE
              MOVE NC"×"         TO   MS01-ERR10
     END-IF.
*    エラー区分１１
     IF  SFR-F06B = " "
              MOVE SPACE          TO   MS01-ERR11
     ELSE
              MOVE NC"×"         TO   MS01-ERR11
     END-IF.
*    エラー区分１２
     IF  SFR-F06C = " "
              MOVE SPACE          TO   MS01-ERR12
     ELSE
              MOVE NC"×"         TO   MS01-ERR12
     END-IF.
*    エラー区分１３
     IF  SFR-F06D = " "
              MOVE SPACE          TO   MS01-ERR13
     ELSE
              MOVE NC"×"         TO   MS01-ERR13
     END-IF.
*    エラー区分１４
     IF  SFR-F06E = " "
              MOVE SPACE          TO   MS01-ERR14
     ELSE
              MOVE NC"×"         TO   MS01-ERR14
     END-IF.
*    エラー区分１５
     IF  SFR-F060 = " "
              MOVE NC"○"         TO   MS01-ERR15
     ELSE
              MOVE NC"　"         TO   MS01-ERR15
     END-IF.
*
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
*行カウント
     ADD      1                   TO    LINE-CNT.
*---------------------------------------------------
*明細２行目
*   入荷予定日
     IF       SFR-F05B  NOT NUMERIC
              MOVE  "?????"       TO   MS02-YOTEIX
     ELSE
              MOVE  SFR-F05B      TO   MS02-YOTEI
     END-IF.
*   仕入先
     MOVE     SFR-F05C            TO   MS02-SIIRESAKI.
*   棚番
     MOVE     SFR-F05D            TO   MS02-TANA.
*   発注数
     MOVE     "?????????"         TO   MS02-HACX.
     IF       SFR-F05E2 NOT NUMERIC
              MOVE    "?????????" TO   MS02-HACX
     ELSE
              IF      SFR-F05E1     =    "-"
                      COMPUTE  WK-SUURYO   =   SFR-F05E2 * -1
                      MOVE     WK-SUURYO  TO   MS02-HACU
              ELSE
                      MOVE     SFR-F05E2  TO   MS02-HACU
              END-IF
     END-IF.
*   ＭＳＧ
     MOVE     SFR-F05F            TO   MS02-MSG.
*
*    明細印刷
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
*行カウント
     ADD      2                   TO   LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数表示
*  ＝振替EXCEL取込ファイル読込件数
     DISPLAY "SFRWKL1 PRINT-CNT = " SFRWKL1-READ-CNT UPON CONS.
*
     CLOSE     SFRWKL1  TANMS1  PRTF.
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