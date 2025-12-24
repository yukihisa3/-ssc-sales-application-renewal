****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　テクノス取込管理リスト            *
*    モジュール名　　　　：　テクノス取込管理リスト            *
*    作成日／更新日　　　：　2011/12/10                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタより範囲を受取、テクノス　*
*                            取込管理リストを発行する。　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3743L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/12/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*テクノス取込管理リスト
     SELECT   TECKANF   ASSIGN    TO        DA-01-VI-TECKANL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       TEC-F01   TEC-F02
                                            TEC-F03   TEC-F04
                                            TEC-F05   TEC-F06
                                            TEC-F07
                        FILE  STATUS   IS   TEC-STATUS.
*作場マスタ
     SELECT   SAKUBAF   ASSIGN    TO        DA-01-VI-SAKUBAL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SKB-F01
                        FILE  STATUS   IS   SKB-STATUS.
*担当者マスタ
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TAN-F01  TAN-F02
                        FILE  STATUS   IS   TAN-STATUS.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    テクノス取込管理ファイル
******************************************************************
 FD  TECKANF            LABEL RECORD   IS   STANDARD.
     COPY     TECKANF   OF        XFDLIB
              JOINING   TEC       PREFIX.
******************************************************************
*    作場マスタ
******************************************************************
 FD  SAKUBAF            LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAF   OF        XFDLIB
              JOINING   SKB       PREFIX.
******************************************************************
*    担当者マスタ
******************************************************************
 FD  HTANMS             LABEL RECORD   IS   STANDARD.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC                 PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  TECKANF-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  SAKUBAF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HTANMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*日付編集
 01  WK-HIDUKE.
     03  WK-HIDUKE-1         PIC  9(04)     VALUE  ZERO.
     03  WK-HIDUKE-X         PIC  X(01)     VALUE  SPACE.
     03  WK-HIDUKE-2         PIC  9(02)     VALUE  ZERO.
     03  WK-HIDUKE-Y         PIC  X(01)     VALUE  SPACE.
     03  WK-HIDUKE-3         PIC  9(02)     VALUE  ZERO.
*時刻編集１
 01  WK-JIKAN1.
     03  WK-JIKAN1-1         PIC  9(02)     VALUE  ZERO.
     03  WK-JIKAN1-X         PIC  X(01)     VALUE  SPACE.
     03  WK-JIKAN1-2         PIC  9(02)     VALUE  ZERO.
     03  WK-JIKAN1-Y         PIC  X(01)     VALUE  SPACE.
     03  WK-JIKAN1-3         PIC  9(02)     VALUE  ZERO.
*時刻編集２
 01  WK-JIKAN2.
     03  WK-JIKAN2-1         PIC  9(02)     VALUE  ZERO.
     03  WK-JIKAN2-X         PIC  X(01)     VALUE  SPACE.
     03  WK-JIKAN2-2         PIC  9(02)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*ステータス
 01  WK-ST.
     03  TEC-STATUS        PIC  X(02).
     03  SKB-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3743L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3743L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3743L".
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
     03  FILLER         CHARACTER  TYPE YB-21.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "SSY3743L".
         05  FILLER          PIC  X(30)     VALUE  SPACE.
         05  FILLER          PIC  N(15)     VALUE
         NC"＜　テクノス取込管理リスト　＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(29)     VALUE  SPACE.
         05  HD00-YYYY       PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD00-MM         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD00-DD         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD00-PCNT       PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"取込日付".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"取込時刻".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"管理番号".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(13)     VALUE
             NC"＜バッチ_－－－－－－－＞".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE NC"出荷場情報".
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  FILLER          PIC  N(02)     VALUE  NC"直送".
         05  FILLER          PIC  X(01)     VALUE  ")".
         05  FILLER          PIC  X(16)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"確定件数".
         05  FILLER          PIC  X(06)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"箱件数".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE NC"取込担当者".
 01  MS01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-HIDUKE     PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-JIKAN      PIC  X(08).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS01-KANRI      PIC  9(08).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS01-BACHI-DATE PIC  X(10).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-BACHI-TIME PIC  X(05).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-BACHI-TORI PIC  9(08).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SOKCD      PIC  X(02).
         05  FILLER          PIC  X(01).
         05  MS01-SOKNM      PIC  N(10).
         05  MS01-TYOKU1     PIC  X(01).
         05  MS01-TYOKUS     PIC  9(02).
         05  MS01-TYOKU2     PIC  X(01).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-KAKUTEI    PIC  ZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-HAKO       PIC  ZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-TANCD      PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TANNM      PIC  N(10).
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-TRDATES       PIC   9(08).
 01  PARA-TRTIMES       PIC   9(06).
 01  PARA-TRDATEE       PIC   9(08).
 01  PARA-TRTIMEE       PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-TRDATES
                                       PARA-TRTIMES
                                       PARA-TRDATEE
                                       PARA-TRTIMEE.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TECKANF.
     MOVE      "TECKANL1"   TO   AB-FILE.
     MOVE      TEC-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SAKUBAF.
     MOVE      "SAKUBAL1"   TO   AB-FILE.
     MOVE      SKB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTANMS.
     MOVE      "TANMS1  "   TO   AB-FILE.
     MOVE      TAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
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
     OPEN     INPUT     TECKANF  SAKUBAF  HTANMS.
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
*ヘッダ行出力の為、行カウント制御
     MOVE        99             TO   LINE-CNT.
*テクノス取込管理Ｆスタート
     PERFORM  TECKANF-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　出力対象データ無１　＃＃"  UPON CONS
          MOVE "END"            TO   END-FLG
          GO                    TO   INIT-EXIT
     END-IF.
*テクノス取込管理Ｆ読込
     PERFORM TECKANF-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　出力対象データ無２　＃＃"  UPON CONS
          MOVE "END"            TO   END-FLG
          GO                    TO   INIT-EXIT
     END-IF.
*取引日付／時刻を保存
     MOVE TEC-F01               TO   WK-TRDATE.
     MOVE TEC-F02               TO   WK-TRTIME.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    テクノス取込管理Ｆスタート
****************************************************************
 TECKANF-START-SEC          SECTION.
*
     MOVE    "TECKANF-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   TEC-REC.
     INITIALIZE                        TEC-REC.
*
     MOVE     PARA-TRDATES        TO   TEC-F01.
     MOVE     PARA-TRTIMES        TO   TEC-F02.
*
     START  TECKANF  KEY  IS  >=  TEC-F01  TEC-F02  TEC-F03
                                  TEC-F04  TEC-F05  TEC-F06
                                  TEC-F07
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 TECKANF-START-EXIT.
     EXIT.
*
****************************************************************
*    テクノス取込管理Ｆ読込
****************************************************************
 TECKANF-READ-SEC           SECTION.
*
     MOVE    "TECKANF-READ-SEC"   TO   S-NAME.
*
     READ     TECKANF  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   TECKANF-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   TECKANF-READ-CNT.
*取込日付チェック
     IF       TEC-F01  >=  PARA-TRDATES
     AND      TEC-F01  <=  PARA-TRDATEE
              CONTINUE
     ELSE
              MOVE     "END"      TO   END-FLG
              GO                  TO   TECKANF-READ-EXIT
     END-IF.
*
     IF       TEC-F02  >=  PARA-TRTIMES
     AND      TEC-F02  <=  PARA-TRTIMEE
              CONTINUE
     ELSE
              GO                  TO   TECKANF-READ-SEC
     END-IF.
*
 TECKANF-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*改頁チェック
     IF   LINE-CNT  >  60
**********ヘッダ行印字
          PERFORM HEAD-WT-SEC
     END-IF.
*明細出力
     PERFORM MEISAI-WT-SEC.
*
     PERFORM  TECKANF-READ-SEC.
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
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD00    AFTER  2.
     WRITE    PRT-REC       FROM  P-LINE2 AFTER  2.
     WRITE    PRT-REC       FROM  HD01    AFTER  1.
     WRITE    PRT-REC       FROM  P-LINE2 AFTER  1.
*****MOVE     SPACE               TO    PRT-REC.
*****WRITE    PRT-REC       AFTER  1.
*行カウント
*****MOVE     7                   TO    LINE-CNT.
     MOVE     6                   TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*    改頁判定
     IF       LINE-CNT  >   60
              PERFORM HEAD-WT-SEC
     END-IF.
*    区切出力
     IF  TEC-F01  =  WK-TRDATE
     AND TEC-F02  =  WK-TRTIME
         CONTINUE
     ELSE
         WRITE  PRT-REC  FROM  P-LINE1 AFTER  1
         ADD  1                   TO   LINE-CNT
         MOVE TEC-F01             TO   WK-TRDATE
         MOVE TEC-F02             TO   WK-TRTIME
     END-IF.
*    取込日付
     MOVE     TEC-F01(1:4)        TO   WK-HIDUKE-1.
     MOVE     TEC-F01(5:2)        TO   WK-HIDUKE-2.
     MOVE     TEC-F01(7:2)        TO   WK-HIDUKE-3.
     MOVE     "/"                 TO   WK-HIDUKE-X.
     MOVE     "/"                 TO   WK-HIDUKE-Y.
     MOVE     WK-HIDUKE           TO   MS01-HIDUKE.
*    取込時刻
     MOVE     TEC-F02(1:2)        TO   WK-JIKAN1-1.
     MOVE     TEC-F02(3:2)        TO   WK-JIKAN1-2.
     MOVE     TEC-F02(5:2)        TO   WK-JIKAN1-3.
     MOVE     ":"                 TO   WK-JIKAN1-X  WK-JIKAN1-Y.
     MOVE     WK-JIKAN1           TO   MS01-JIKAN.
*    管理番号
     MOVE     TEC-F03             TO   MS01-KANRI.
*    バッチ日付
     MOVE     TEC-F04(1:4)        TO   WK-HIDUKE-1.
     MOVE     TEC-F04(5:2)        TO   WK-HIDUKE-2.
     MOVE     TEC-F04(7:2)        TO   WK-HIDUKE-3.
     MOVE     "/"                 TO   WK-HIDUKE-X.
     MOVE     "/"                 TO   WK-HIDUKE-Y.
     MOVE     WK-HIDUKE           TO   MS01-BACHI-DATE.
*    バッチ時刻
     MOVE     TEC-F05(1:2)        TO   WK-JIKAN2-1.
     MOVE     TEC-F05(3:2)        TO   WK-JIKAN2-2.
     MOVE     ":"                 TO   WK-JIKAN2-X.
     MOVE     WK-JIKAN2           TO   MS01-BACHI-TIME.
*    取引先ＣＤ
     MOVE     TEC-F06             TO   MS01-BACHI-TORI.
*    出荷場所
     MOVE     TEC-F07             TO   MS01-SOKCD.
*    作場マスタ索引
     MOVE     "("                 TO   MS01-TYOKU1.
     MOVE     ")"                 TO   MS01-TYOKU2.
     MOVE     TEC-F07             TO   SKB-F01.
     PERFORM  SAKUBAF-READ-SEC.
     IF  SAKUBAF-INV-FLG = SPACE
         MOVE SKB-F02             TO   MS01-SOKNM
         MOVE SKB-F04             TO   MS01-TYOKUS
     ELSE
         MOVE NC"作場Ｍ登録必要！！！"
                                  TO   MS01-SOKNM
         MOVE ZERO                TO   MS01-TYOKUS
     END-IF.
*    確定数量
     MOVE     TEC-F08             TO   MS01-KAKUTEI.
*    箱数量
     MOVE     TEC-F09             TO   MS01-HAKO.
*    担当者ＣＤ
     MOVE     TEC-F10             TO   MS01-TANCD.
*    担当者マスタ索引
     MOVE     TEC-F11             TO   TAN-F01.
     MOVE     TEC-F10             TO   TAN-F02.
     PERFORM  HTANMS-READ-SEC.
     IF  HTANMS-INV-FLG = SPACE
              MOVE  TAN-F03       TO   MS01-TANNM
     ELSE
              MOVE  ALL NC"＊"    TO   MS01-TANNM
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
*行カウント
     ADD      1                   TO    LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　作場マスタ索引
****************************************************************
 SAKUBAF-READ-SEC      SECTION.
*
     MOVE     "SAKUBAF-READ-SEC"  TO    S-NAME.
*
     READ  SAKUBAF
           INVALID     MOVE "INV" TO    SAKUBAF-INV-FLG
           NOT INVALID MOVE SPACE TO    SAKUBAF-INV-FLG
     END-READ.
*
 SAKUBAF-READ-EXIT.
     EXIT.
****************************************************************
*　　担当者マスタ索引
****************************************************************
 HTANMS-READ-SEC       SECTION.
*
     MOVE     "HTANMS-READ-SEC"   TO    S-NAME.
*
     READ  HTANMS
           INVALID     MOVE "INV" TO    HTANMS-INV-FLG
           NOT INVALID MOVE SPACE TO    HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*区切出力
     IF  PAGE-CNT > ZERO
         WRITE  PRT-REC  FROM  P-LINE1 AFTER  1
     END-IF.
*件数印字
*テクノス取込管理Ｆ読込
     DISPLAY NC"読込件数　＝　" TECKANF-READ-CNT UPON CONS.
*出力枚数出力
     DISPLAY NC"出力枚数　＝　" PAGE-CNT         UPON CONS.
*
     CLOSE     TECKANF  SAKUBAF  HTANMS  PRTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
