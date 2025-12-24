****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援　　　　            *
*    モジュール名　　　　：　作場・数量一括変更　　　　　　　　*
*    　　　　　　　　　　　　　　取込エラーリスト　　　　　　　*
*    作成日／作成者　　　：　2019/02/19 INOUE                  *
*    処理概要　　　　　　：　取込チェックにてエラーとなった　　*
*                            データをリスト出力する。　　　　　*
*    更新日／更新者　　　：　                                  *
*                            　　　　　　　　　　              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5150L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/02/19.
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
*一括変更データ取込Ｆ
     SELECT   CHGXXXT2  ASSIGN    TO        DA-01-VI-CHGXXXT2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       CHG-F060
                                            CHG-F0501
                                            CHG-F05021
                                            CHG-F05022
                                            CHG-F05023
                                            CHG-F0503
                                            CHG-F0504
                                            CHG-F0505
                                            CHG-F0506
                        FILE  STATUS   IS   CHG-STATUS.
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
*    一括変更データ取込Ｆ
 FD  CHGXXXT2            LABEL RECORD   IS   STANDARD.
     COPY     CHGXXXT2   OF        XFDLIB
              JOINING   CHG       PREFIX.
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
 01  CHGXXXT2-READ-CNT       PIC  9(07)     VALUE  ZERO.
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
     03  CHG-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SAK-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY5150L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5150L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY5150L".
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
         05  FILLER          PIC  X(08)     VALUE  "SSY5150L".
         05  FILLER          PIC  X(24)     VALUE  SPACE.
         05  FILLER          PIC  N(19)     VALUE
         NC"＜作場・数量一括変更取込エラーリスト＞".
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
         05  FILLER          PIC  N(07)     VALUE
             NC"エラー情報　→".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(08)     VALUE
             NC"_倉庫ＣＤ誤り　".
         05  FILLER          PIC  N(07)     VALUE
             NC"_データ重複　".
         05  FILLER          PIC  N(08)     VALUE
             NC"_売上伝票なし　".
         05  FILLER          PIC  N(08)     VALUE
             NC"_基本情報なし　".
         05  FILLER          PIC  N(04)     VALUE
             NC"_　　　".
         05  FILLER          PIC  N(04)     VALUE
             NC"_　　　".
         05  FILLER          PIC  N(04)     VALUE
             NC"_　　　".
         05  FILLER          PIC  N(04)     VALUE
             NC"_　　　".
         05  FILLER          PIC  N(04)     VALUE
             NC"_　　　".
         05  FILLER          PIC  N(04)     VALUE
             NC"_　　　".
*
 01  HD02.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(106)    VALUE  SPACE.
         05  FILLER          PIC  N(10)
                             VALUE NC"＜　エラー情報　　＞".
 01  HD04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"管理番号".
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE  NC"バッチ日付".
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"時刻".
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"取引先".
         05  FILLER          PIC  X(05)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"納品日".
         05  FILLER          PIC  X(05)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"店舗".
         05  FILLER          PIC  X(03)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"伝票番号".
         05  FILLER          PIC  X(03)   VALUE  " - ".
         05  FILLER          PIC  N(01)   VALUE  NC"行".
         05  FILLER          PIC  X(04)   VALUE  "  ( ".
         05  FILLER          PIC  N(03)   VALUE  NC"倉→倉".
         05  FILLER          PIC  X(04)   VALUE  " )  ".
         05  FILLER          PIC  X(03)   VALUE  "(  ".
         05  FILLER          PIC  N(02)   VALUE  NC"数量".
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE  NC"→".
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"数量".
         05  FILLER          PIC  X(04)   VALUE  "  ) ".
         05  FILLER          PIC  N(10)
                             VALUE NC"__________".
*
 01  MS01.
     03  FILLER              CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-KANRINO    PIC  9(08).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-BDATE      PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BTIME      PIC  X(05).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BTORI      PIC  9(08).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-NDATE      PIC  X(10).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-TENCD      PIC  ZZZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-DENNO      PIC  9(09).
         05  FILLER          PIC  X(03)     VALUE  " - ".
         05  MS01-GYONO      PIC  9(02).
         05  FILLER          PIC  X(04)     VALUE  "  ( ".
         05  MS01-SOKCD      PIC  X(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SAKUCD     PIC  X(02).
         05  FILLER          PIC  X(06)     VALUE  " )  ( ".
         05  MS01-HSUU       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-TSUU       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(03)     VALUE  " ) ".
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
                        PROCEDURE   CHGXXXT2.
     MOVE      "CHGXXXT2"   TO   AB-FILE.
     MOVE      CHG-STATUS   TO   AB-STS.
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
     OPEN     INPUT     CHGXXXT2
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
*一括変更データ取込Ｆスタート
     PERFORM  CHGXXXT2-START-SEC.
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
*一括変更データ取込Ｆ読込
     PERFORM CHGXXXT2-READ-SEC.
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
*    一括変更データ取込Ｆスタート
****************************************************************
 CHGXXXT2-START-SEC          SECTION.
*
     MOVE    "CHGXXXT2-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   CHG-REC.
     INITIALIZE                        CHG-REC.
*
     MOVE     "1"                 TO   CHG-F060.
     MOVE     ZERO                TO   CHG-F0501.
     MOVE     ZERO                TO   CHG-F05021.
     MOVE     ZERO                TO   CHG-F05022.
     MOVE     ZERO                TO   CHG-F05023.
     MOVE     SPACE               TO   CHG-F0503.
     MOVE     ZERO                TO   CHG-F0504.
     MOVE     ZERO                TO   CHG-F0505.
     MOVE     ZERO                TO   CHG-F0506.
*
     START  CHGXXXT2  KEY  IS  >=      CHG-F060   CHG-F0501
                                       CHG-F05021 CHG-F05022
                                       CHG-F05023 CHG-F0503
                                       CHG-F0504  CHG-F0505
                                       CHG-F0506
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 CHGXXXT2-START-EXIT.
     EXIT.
*
****************************************************************
*    一括変更データ取込Ｆ読込
****************************************************************
 CHGXXXT2-READ-SEC           SECTION.
*
     MOVE    "CHGXXXT2-READ-SEC"   TO   S-NAME.
*
     READ     CHGXXXT2  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   CHGXXXT2-READ-EXIT
     END-READ.
*エラー区分チェック
     IF       CHG-F060  NOT =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   CHGXXXT2-READ-EXIT
     END-IF.
*件数カウント
     ADD      1                   TO   CHGXXXT2-READ-CNT.
*
 CHGXXXT2-READ-EXIT.
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
     PERFORM  CHGXXXT2-READ-SEC.
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
*   管理番号
     MOVE  CHG-F0501              TO   MS01-KANRINO.
*   バッチ日付
     MOVE     CHG-F05021(1:4)     TO   MS01-BDATE(1:4).
     MOVE     "/"                 TO   MS01-BDATE(5:1).
     MOVE     CHG-F05021(5:2)     TO   MS01-BDATE(6:2).
     MOVE     "/"                 TO   MS01-BDATE(8:1).
     MOVE     CHG-F05021(7:2)     TO   MS01-BDATE(9:2).
*   時刻
     MOVE     CHG-F05022(1:2)     TO   MS01-BTIME(1:2).
     MOVE     ":"                 TO   MS01-BTIME(3:1).
     MOVE     CHG-F05022(3:2)     TO   MS01-BTIME(4:2).
*   取引先
     MOVE     CHG-F05023          TO   MS01-BTORI.
*   納品日
     MOVE     CHG-F0503(1:4)      TO   MS01-NDATE(1:4).
     MOVE     "/"                 TO   MS01-NDATE(5:1).
     MOVE     CHG-F0503(5:2)      TO   MS01-NDATE(6:2).
     MOVE     "/"                 TO   MS01-NDATE(8:1).
     MOVE     CHG-F0503(7:2)      TO   MS01-NDATE(9:2).
*   店舗
     MOVE     CHG-F0504           TO   MS01-TENCD.
*   伝票番号
     MOVE     CHG-F0505           TO   MS01-DENNO.
*   行番号
     MOVE     CHG-F0506           TO   MS01-GYONO.
*   倉庫（元）
     MOVE     CHG-F0509           TO   MS01-SOKCD.
*   変更作場
     MOVE     CHG-F050A           TO   MS01-SAKUCD.
*   発注数
     MOVE     CHG-F0507           TO   MS01-HSUU.
*   訂正数
     MOVE     CHG-F0508           TO   MS01-TSUU.
*   エラー区分１
     IF  CHG-F061 = " "
              MOVE SPACE          TO   MS01-ERR1
     ELSE
              MOVE NC"×"         TO   MS01-ERR1
     END-IF.
*    エラー区分２
     IF  CHG-F062 = " "
              MOVE SPACE          TO   MS01-ERR2
     ELSE
              MOVE NC"×"         TO   MS01-ERR2
     END-IF.
*    エラー区分３
     IF  CHG-F063 = " "
              MOVE SPACE          TO   MS01-ERR3
     ELSE
              MOVE NC"×"         TO   MS01-ERR3
     END-IF.
*    エラー区分４
     IF  CHG-F064 = " "
              MOVE SPACE          TO   MS01-ERR4
     ELSE
              MOVE NC"×"         TO   MS01-ERR4
     END-IF.
*    エラー区分５
     IF  CHG-F065 = " "
              MOVE SPACE          TO   MS01-ERR5
     ELSE
              MOVE NC"×"         TO   MS01-ERR5
     END-IF.
*    エラー区分６
     IF  CHG-F066 = " "
              MOVE SPACE          TO   MS01-ERR6
     ELSE
              MOVE NC"×"         TO   MS01-ERR6
     END-IF.
*    エラー区分７
     IF  CHG-F067 = " "
              MOVE SPACE          TO   MS01-ERR7
     ELSE
              MOVE NC"×"         TO   MS01-ERR7
     END-IF.
*    エラー区分８
     IF  CHG-F068 = " "
              MOVE SPACE          TO   MS01-ERR8
     ELSE
              MOVE NC"×"         TO   MS01-ERR8
     END-IF.
*    エラー区分９
     IF  CHG-F069 = " "
              MOVE SPACE          TO   MS01-ERR9
     ELSE
              MOVE NC"×"         TO   MS01-ERR9
     END-IF.
*    エラー区分１０
     IF  CHG-F06A = " "
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
*一括変更データ取込Ｆ読込
     DISPLAY "CHGXXXT2 PRINT-CNT = " CHGXXXT2-READ-CNT UPON CONS.
*
     CLOSE     CHGXXXT2  TANMS1  PRTF.
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