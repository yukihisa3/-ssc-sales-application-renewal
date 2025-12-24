****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　            *
*    モジュール名　　　　：　ナフコ店舗パターンマスタ取込　*
*    　　　　　　　　　　：　チェックリスト　　　　　　　　　*
*    作成日／作成者　　　：　2016/08/18 MIURA                  *
*    処理概要　　　　　　：　取込チェックにてエラーとなった　　*
*                            データをリスト出力する。　　　　　*
*    更新日／更新者　　　：　2016/08/18 MIURA                  *
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3931L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/08/18.
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
*ナフコ店舗パターンファイル
     SELECT   NFTEPWK1  ASSIGN    TO        DA-01-VI-NFTEPWK1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       EXL-F060 EXL-F051
                                            EXL-F052 EXL-F053
                        FILE  STATUS   IS   EXL-STATUS.
*担当者マスタ
     SELECT     TANMS1       ASSIGN    TO       DA-01-VI-TANMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01  TAN-F02
                             FILE      STATUS   TAN-STATUS.
*ナフコ店舗マスタ
     SELECT      NFTENMS1    ASSIGN    TO       DA-01-VI-NFTENMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      MS1-F01 MS1-F02
                             FILE      STATUS   MS1-STATUS.
*ナフコ店舗分類パターンマスタ
     SELECT      NFSHBPL1    ASSIGN    TO       DA-01-VI-NFSHBPL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      PL1-F01 PL1-F03
                             FILE      STATUS   PL1-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*    ナフコ店舗パターンファイル
 FD  NFTEPWK1            LABEL RECORD   IS   STANDARD.
     COPY     NFTEPWK1   OF        XFDLIB
              JOINING   EXL       PREFIX.
*担当者マスタ
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*ナフコ店舗マスタ
 FD  NFTENMS1.
     COPY        NFTENMS1    OF        XFDLIB
     JOINING     MS1         AS        PREFIX.
*ナフコ店舗分類パターンマスタ
 FD  NFSHBPL1.
     COPY        NFSHBPL1    OF        XFDLIB
     JOINING     PL1         AS        PREFIX.
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
 01  NFTEPWK1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  MS1-INV-FLG             PIC  X(03)     VALUE  SPACE.
 01  PL1-INV-FLG             PIC  X(03)     VALUE  SPACE.
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
     03  TAN-STATUS        PIC  X(02).
     03  MS1-STATUS        PIC  X(02).
     03  PL1-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3931L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3931L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3931L".
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
         05  FILLER          PIC  X(08)     VALUE  "SSY3931L".
*        05  FILLER          PIC  X(27)     VALUE  SPACE.
*        05  FILLER          PIC  N(22)     VALUE
*        NC"＜ナフコ店舗パターン取込チェックリスト＞".
         05  FILLER          PIC  X(15)     VALUE  SPACE.
         05  FILLER          PIC  N(25)     VALUE
         NC"＜ナフコ店舗パターンマスタ取込チェックリスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(04)     VALUE  SPACE.
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
         05  FILLER          PIC  N(06)
                             VALUE  NC"取込担当者：".
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
         05  FILLER          PIC  N(06)
                             VALUE  NC"取込日付　：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD01-YYYY       PIC  9(04).
         05  FILLER          PIC  X(01)     VALUE  "/".
         05  HD01-MM         PIC  Z9.
         05  FILLER          PIC  X(01)     VALUE  "/".
         05  HD01-DD         PIC  Z9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)
                             VALUE  NC"取込時刻：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD01-HH         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD01-SS         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD01-MS         PIC  9(02).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"エラー情報→".
         05  FILLER          PIC  N(05)     VALUE
             NC"_ナ店舗Ｍ".
         05  FILLER          PIC  N(06)     VALUE
             NC"_商品分類パ".
         05  FILLER          PIC  N(04)     VALUE
             NC"_整合性".
         05  FILLER          PIC  N(05)     VALUE
             NC"_必須項目".
         05  FILLER          PIC  N(03)     VALUE
             NC"_　　".
         05  FILLER          PIC  N(03)     VALUE
             NC"_　　".
         05  FILLER          PIC  N(03)     VALUE
             NC"_　　".
         05  FILLER          PIC  N(03)     VALUE
             NC"_　　".
         05  FILLER          PIC  N(03)     VALUE
             NC"_　　".
         05  FILLER          PIC  N(03)     VALUE
             NC"_正常".
*
 01  HD02.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(76)    VALUE  SPACE.
*        05  FILLER          PIC  N(10)
*                            VALUE NC"＜　エラー情報　　＞".
         05  FILLER          PIC  N(10)
                             VALUE NC"＜　－－－　エラー情".
         05  FILLER          PIC  N(09)
                             VALUE NC"報　－－－－　＞　".
 01  HD04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER        PIC  N(04)  VALUE  NC"店舗ＣＤ".
         05  FILLER          PIC  X(15)     VALUE  SPACE.
         05  FILLER        PIC  N(06)  VALUE  NC"商品分類ＣＤ".
         05  FILLER          PIC  X(22)     VALUE  SPACE.
         05  FILLER        PIC  N(06)  VALUE  NC"パターンＣＤ".
         05  FILLER          PIC  X(07)     VALUE  SPACE.
*        05  FILLER          PIC  N(10)
*                            VALUE NC"__________".
         05  FILLER          PIC  N(10)
                             VALUE NC"_　_　_　_　_　".
         05  FILLER          PIC  N(10)
                             VALUE NC"_　_　_　_　_　".
*
 01  MS01.
     03  FILLER              CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TEMCD      PIC  9(05).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-TEMNM      PIC  N(10).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SHOBNRUI   PIC  X(08).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
*        05  MS01-SHOBNMEI   PIC  N(15).
         05  MS01-SHOBNMEI   PIC  N(16).
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  MS01-PTNCD      PIC  X(02).
*        05  FILLER          PIC  X(14)     VALUE  SPACE.
         05  FILLER          PIC  X(12)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  MS01-ERR1       PIC  N(01).
         05  FILLER          PIC  N(01)     VALUE  SPACE.
         05  MS01-ERR2       PIC  N(01).
         05  FILLER          PIC  N(01)     VALUE  SPACE.
         05  MS01-ERR3       PIC  N(01).
         05  FILLER          PIC  N(01)     VALUE  SPACE.
         05  MS01-ERR4       PIC  N(01).
         05  FILLER          PIC  N(01)     VALUE  SPACE.
         05  MS01-ERR5       PIC  N(01).
         05  FILLER          PIC  N(01)     VALUE  SPACE.
         05  MS01-ERR6       PIC  N(01).
         05  FILLER          PIC  N(01)     VALUE  SPACE.
         05  MS01-ERR7       PIC  N(01).
         05  FILLER          PIC  N(01)     VALUE  SPACE.
         05  MS01-ERR8       PIC  N(01).
         05  FILLER          PIC  N(01)     VALUE  SPACE.
         05  MS01-ERR9       PIC  N(01).
         05  FILLER          PIC  N(01)     VALUE  SPACE.
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
                        PROCEDURE   NFTEPWK1.
     MOVE      "NFTEPWK1"   TO   AB-FILE.
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
                        PROCEDURE   NFTENMS1.
     MOVE      "NFTENMS1"   TO   AB-FILE.
     MOVE      MS1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSHBPL1.
     MOVE      "NFSHBPL1"   TO   AB-FILE.
     MOVE      PL1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
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
     OPEN     INPUT     NFTEPWK1 NFTENMS1
                        TANMS1   NFSHBPL1.
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
*ナフコ店舗パターンマスタ取込ファイルスタート
     PERFORM  NFTEPWK1-START-SEC.
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
*ナフコ店舗パターンマスタ取込ファイル読込
     PERFORM NFTEPWK1-READ-SEC.
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
* ナフコ店舗パターンマスタ取込ファイルスタート
****************************************************************
 NFTEPWK1-START-SEC          SECTION.
*
     MOVE    "NFTEPWK1-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   EXL-REC.
     INITIALIZE                        EXL-REC.
*
*    MOVE     "1"                 TO   EXL-F060.
     MOVE     ZERO                TO   EXL-F051.
     MOVE     ZERO                TO   EXL-F052.
     MOVE     ZERO                TO   EXL-F053.
*
     START  NFTEPWK1  KEY  IS  >=      EXL-F060 EXL-F051
                                       EXL-F052 EXL-F053
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 NFTEPWK1-START-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ店舗パターンマスタ読込
****************************************************************
 NFTEPWK1-READ-SEC           SECTION.
*
     MOVE    "NFTEPWK1-READ-SEC"   TO   S-NAME.
*
     READ     NFTEPWK1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFTEPWK1-READ-EXIT
     END-READ.
*エラー区分チェック
     IF       EXL-F060  NOT =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFTEPWK1-READ-EXIT
     END-IF.
*件数カウント
     ADD      1                   TO   NFTEPWK1-READ-CNT.
*
 NFTEPWK1-READ-EXIT.
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
     PERFORM  NFTEPWK1-READ-SEC.
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
*    取込日付セット
     MOVE     EXL-F01(1:4)      TO   HD01-YYYY.
     MOVE     EXL-F01(5:2)      TO   HD01-MM.
     MOVE     EXL-F01(7:2)      TO   HD01-DD.
*    システム時刻セット
     MOVE     EXL-F02(1:2)     TO   HD01-HH.
     MOVE     EXL-F02(3:2)     TO   HD01-SS.
     MOVE     EXL-F02(5:2)     TO   HD01-MS.
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
*   店舗コード
     IF       EXL-F051  NOT NUMERIC
              MOVE  "????????"    TO   MS01-TEMCD
     ELSE
              MOVE  EXL-F051      TO   MS01-TEMCD
     END-IF.
*    店舗名　　
     MOVE     137607              TO   MS1-F01.
     MOVE     EXL-F051            TO   MS1-F02.
     PERFORM  MS1-READ-SEC.
     IF  MS1-INV-FLG = SPACE
               MOVE  MS1-F05      TO   MS01-TEMNM
     ELSE
               MOVE  ALL NC"？"   TO   MS01-TEMNM
     END-IF.
*    商品分類コード
     MOVE     EXL-F052            TO   MS01-SHOBNRUI.
*    商品分類名
     MOVE     EXL-F052            TO   PL1-F01.
     MOVE     EXL-F053            TO   PL1-F03.
     PERFORM  PL1-READ-SEC.
     IF  PL1-INV-FLG = SPACE
               MOVE  PL1-F02      TO   MS01-SHOBNMEI
     ELSE
               MOVE  ALL NC"？"   TO   MS01-SHOBNMEI
     END-IF.
*   パターンコード
     MOVE     EXL-F053            TO   MS01-PTNCD.
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
              MOVE NC"〇"         TO   MS01-ERR10
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
*ナフコ店舗パターンマスタ
     DISPLAY "NFTEPWK1 PRINT-CNT = " NFTEPWK1-READ-CNT UPON CONS.
*
     CLOSE     NFTEPWK1  TANMS1 NFTENMS1 NFSHBPL1 PRTF.
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
****************************************************************
*    店舗マスタ検索
****************************************************************
 MS1-READ-SEC               SECTION.
     MOVE     "MS1-READ-SEC" TO   S-NAME.
*
     READ     NFTENMS1
       INVALID
              MOVE "INV"     TO   MS1-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   MS1-INV-FLG
     END-READ.
 MS1-READ-EXIT.
     EXIT.
****************************************************************
*    商品分類パターンマスタ検索
****************************************************************
 PL1-READ-SEC               SECTION.
     MOVE     "PL1-READ-SEC" TO   S-NAME.
*
     READ     NFSHBPL1
       INVALID
              MOVE "INV"     TO   PL1-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   PL1-INV-FLG
     END-READ.
 PL1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
