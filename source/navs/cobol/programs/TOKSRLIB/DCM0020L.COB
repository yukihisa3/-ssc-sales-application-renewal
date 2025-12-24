****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　　　　　　　　            *
*    モジュール名　　　　：　実行履歴リスト発行　　　　　　    *
*    作成日／作成者　　　：　2022/05/16 INOUE                  *
*    処理概要　　　　　　：　パラメタ範囲の実行履歴リスト　　　*
*                            を出力する。　　　　　　　　　　　*
*    変更日／変更者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            DCM0020L.
*                  流用:SZI0090L
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/05/16.
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
*実行履歴ファイル
     SELECT   JIKRERL1  ASSIGN    TO        DA-01-VI-JIKRERL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JIK-F01
                                            JIK-F02
                                            JIK-F03
                                            JIK-F04
                                            JIK-F05
                                            JIK-F06
                                            JIK-F07
                        FILE  STATUS   IS   JIK-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*    実行履歴ファイル
 FD  JIKRERL1           LABEL RECORD   IS   STANDARD.
     COPY     JIKRERL1  OF        XFDLIB
              JOINING   JIK       PREFIX.
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
 01  JIKRERL1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  ZERO.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
*ステータス
 01  WK-ST.
     03  KAK-STATUS          PIC  X(02).
     03  JIK-STATUS          PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
     03  SAK-STATUS          PIC  X(02).
     03  TAN-STATUS          PIC  X(02).
     03  MEI-STATUS          PIC  X(02).
     03  PRT-STATUS          PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  ST-PG           PIC   X(08)  VALUE "DCM0020L".
         05  FILLER          PIC   X(11)  VALUE
                                          " START *** ".
     03  MSG-END.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "DCM0020L".
         05  FILLER          PIC   X(11)  VALUE
                                          " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "DCM0020L".
         05  FILLER          PIC   X(11)  VALUE
                                          " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  AB-FILE         PIC   X(08).
         05  FILLER          PIC   X(06)  VALUE " ST = ".
         05  AB-STS          PIC   X(02).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(07)  VALUE " SEC = ".
         05  S-NAME          PIC   X(30).
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
*    03  FILLER         CHARACTER  TYPE YB-22.
     03  FILLER         CHARACTER  TYPE YB-21.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "DCM0020L".
         05  FILLER          PIC  X(35)     VALUE  SPACE.
         05  FILLER          PIC  N(11)     VALUE
         NC"＜　実行履歴リスト　＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(30)     VALUE  SPACE.
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
                             VALUE  NC"起動日付範囲".
         05  FILLER          PIC  N(01)     VALUE  NC"：".
         05  HD000-DATEF     PIC  9(08).
         05  FILLER          PIC  N(01)     VALUE  NC"～".
         05  HD000-DATET     PIC  9(08).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(06)
                             VALUE  NC"起動時刻範囲".
         05  FILLER          PIC  N(01)     VALUE  NC"：".
         05  HD000-TIMEF     PIC  9(06).
         05  FILLER          PIC  N(01)     VALUE  NC"～".
         05  HD000-TIMET     PIC  9(06).
*
 01  HD01.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD02.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"起動日付".
         05  FILLER          PIC  X(01)   VALUE  "-".
         05  FILLER          PIC  N(02)   VALUE  NC"時刻".
         05  FILLER          PIC  X(03)   VALUE  SPACE.
         05  FILLER          PIC  X(05)   VALUE  "WKSTN".
         05  FILLER          PIC  N(01)   VALUE  NC"名".
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE  NC"担".
         05  FILLER          PIC  X(05)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"処理".
         05  FILLER          PIC  X(02)   VALUE  "NO".
         05  FILLER          PIC  X(05)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"実行".
         05  FILLER          PIC  X(02)   VALUE  "NO".
         05  FILLER          PIC  X(03)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"子実行".
         05  FILLER          PIC  X(02)   VALUE  "NO".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"開始日付".
         05  FILLER          PIC  X(01)   VALUE  "-".
         05  FILLER          PIC  N(02)   VALUE  NC"時刻".
         05  FILLER          PIC  X(03)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"終了日付".
         05  FILLER          PIC  X(01)   VALUE  "-".
         05  FILLER          PIC  N(02)   VALUE  NC"時刻".
         05  FILLER          PIC  X(03)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"処理名称".
         05  FILLER          PIC  X(09)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"実行名称".
         05  FILLER          PIC  X(08)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE  NC"結".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"ＳＴ".
*
 01  MS01.
     03  FILLER              CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-KDATE      PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  MS01-KTIME      PIC  9(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-WSID       PIC  X(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TANCD      PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SYORI      PIC  ZZZZZZZZZ9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-JIKKOU     PIC  ZZZZZZZZZ9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-KOJIKKOU   PIC  ZZZZZZZZZ9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-FDATE      PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  MS01-FTIME      PIC  9(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TDATE      PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  MS01-TTIME      PIC  9(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SYORINM    PIC  N(10).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-JIKKOUNM   PIC  N(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-KEKKA      PIC  X(01).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-STATUS     PIC  X(04).
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
             NC"＃　該当する履歴データはありません　！！　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
 01  P-SPACE                PIC  X(01)     VALUE  SPACE.
 01  P-LINE1                PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2                PIC  X(136)    VALUE  ALL   "=".
*時刻編集
 01  SYS-TIME               PIC  9(08).
 01  WK-TIME      REDEFINES SYS-TIME.
   03  WK-TIME-HM           PIC  9(06).
   03  WK-TIME-FIL          PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-KDATEF        PIC   9(08).
 01  PARA-IN-KDATET        PIC   9(08).
 01  PARA-IN-KTIMEF        PIC   9(06).
 01  PARA-IN-KTIMET        PIC   9(06).
 01  PARA-IN-WSIDF         PIC   X(08).
 01  PARA-IN-WSIDT         PIC   X(08).
 01  PARA-IN-TANF          PIC   X(02).
 01  PARA-IN-TANT          PIC   X(02).
 01  PARA-IN-SYORIF        PIC   9(10).
 01  PARA-IN-SYORIT        PIC   9(10).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-KDATEF
                                       PARA-IN-KDATET
                                       PARA-IN-KTIMEF
                                       PARA-IN-KTIMET
                                       PARA-IN-WSIDF
                                       PARA-IN-WSIDT
                                       PARA-IN-TANF
                                       PARA-IN-TANT
                                       PARA-IN-SYORIF
                                       PARA-IN-SYORIT.
*---------------------------------------------------------------*
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JIKRERL1.
     MOVE      "JIKRERL1"   TO   AB-FILE.
     MOVE      JIK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
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
     MOVE     "PROCESS-START"    TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL   END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     JIKRERL1.
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
*実行履歴ファイルスタート
     PERFORM  JIKRERL1-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　履歴データ　　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*実行履歴ファイル読込
     PERFORM JIKRERL1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　履歴データ　　なし　＃＃"  UPON CONS
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
*    実行履歴ファイルスタート
****************************************************************
 JIKRERL1-START-SEC          SECTION.
*
     MOVE    "JIKRERL1-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   JIK-REC.
     INITIALIZE                        JIK-REC.
*
     MOVE     PARA-IN-KDATEF      TO   JIK-F01.
     MOVE     PARA-IN-KTIMEF      TO   JIK-F02.
     MOVE     PARA-IN-WSIDF       TO   JIK-F03.
     MOVE     PARA-IN-TANF        TO   JIK-F04.
     MOVE     PARA-IN-SYORIF      TO   JIK-F05.
     MOVE     ZERO                TO   JIK-F06.
     MOVE     ZERO                TO   JIK-F07.
*
     START  JIKRERL1  KEY  IS  >=      JIK-F01 JIK-F02 JIK-F03
                                       JIK-F04 JIK-F05 JIK-F06
                                       JIK-F07
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 JIKRERL1-START-EXIT.
     EXIT.
*
****************************************************************
*    実行履歴ファイル読込
****************************************************************
 JIKRERL1-READ-SEC           SECTION.
*
     MOVE    "JIKRERL1-READ-SEC"   TO   S-NAME.
*
 JIKRERL1-READ-01.
     READ     JIKRERL1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   JIKRERL1-READ-EXIT
     END-READ.
*
*起動日付範囲
     IF     ( JIK-F01       <  PARA-IN-KDATEF ) OR
            ( JIK-F01       >  PARA-IN-KDATET )
              MOVE     "END"      TO   END-FLG
              GO                  TO   JIKRERL1-READ-EXIT
     END-IF.
*起動時刻範囲
     IF     ( JIK-F02       <  PARA-IN-KTIMEF ) OR
            ( JIK-F02       >  PARA-IN-KTIMET )
              GO                  TO   JIKRERL1-READ-01
     END-IF.
*ＷＫＳＴＮ範囲
     IF     ( JIK-F03       <  PARA-IN-WSIDF  ) OR
            ( JIK-F03       >  PARA-IN-WSIDT  )
              GO                  TO   JIKRERL1-READ-01
     END-IF.
*担当者範囲
     IF     ( JIK-F04       <  PARA-IN-TANF   ) OR
            ( JIK-F04       >  PARA-IN-TANT   )
              GO                  TO   JIKRERL1-READ-01
     END-IF.
*処理ＮＯ範囲
     IF     ( JIK-F05       <  PARA-IN-SYORIF ) OR
            ( JIK-F05       >  PARA-IN-SYORIT )
              GO                  TO   JIKRERL1-READ-01
     END-IF.
*件数カウント
     ADD      1                   TO   JIKRERL1-READ-CNT.
*
 JIKRERL1-READ-EXIT.
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
     PERFORM  JIKRERL1-READ-SEC.
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
*    起動日付範囲
     MOVE     PARA-IN-KDATEF      TO   HD000-DATEF.
     MOVE     PARA-IN-KDATET      TO   HD000-DATET.
*    起動時刻範囲
     MOVE     PARA-IN-KTIMEF      TO   HD000-TIMEF.
     MOVE     PARA-IN-KTIMET      TO   HD000-TIMET.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD000 AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     8                   TO    LINE-CNT.
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
*   起動日付
     MOVE     JIK-F01             TO   MS01-KDATE.
*   起動時刻
     MOVE     JIK-F02             TO   MS01-KTIME.
*   ＷＫＳＴＮ名
     MOVE     JIK-F03             TO   MS01-WSID.
*   担当者
     MOVE     JIK-F04             TO   MS01-TANCD.
*   処理ＮＯ
     MOVE     JIK-F05             TO   MS01-SYORI.
*   実行ＮＯ
     MOVE     JIK-F06             TO   MS01-JIKKOU.
*   子実行ＮＯ
     MOVE     JIK-F07             TO   MS01-KOJIKKOU.
*   開始日付
     MOVE     JIK-F08             TO   MS01-FDATE.
*   開始時刻
     MOVE     JIK-F09             TO   MS01-FTIME.
*   終了日付
     MOVE     JIK-F10             TO   MS01-TDATE.
*   終了時刻
     MOVE     JIK-F11             TO   MS01-TTIME.
*   処理名称
     MOVE     JIK-F12             TO   MS01-SYORINM.
*   実行名称
     MOVE     JIK-F13             TO   MS01-JIKKOUNM.
*   実行結果
     MOVE     JIK-F14             TO   MS01-KEKKA.
*   ステータス
     MOVE     JIK-F15             TO   MS01-STATUS.
*
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
*    行カウント
     ADD      1                   TO    LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC      SECTION.
*
     MOVE    "END-SEC"  TO      S-NAME.
*
*件数表示
     DISPLAY "JIKRERL1 PRINT-CNT = " JIKRERL1-READ-CNT UPON CONS.
*
     CLOSE    JIKRERL1 PRTF.
*
     STOP     RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
