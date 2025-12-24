# DCM0050L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/DCM0050L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＷｅｂＥＤＩ自動化　　            *
*    モジュール名　　　　：　ＷｅｂＥＤＩ自動取込結果リスト    *
*    作成日／作成者　　　：　2022/08/31 TAKAHASHI              *
*    処理概要　　　　　　：　受信制御ファイルを読み、結果リスト*
*                            と結果パラメタを出力する。　　　　*
*    変更日／変更者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            DCM0050L.
*                  流用:DCM0020L
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/08/31.
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
*ＤＣＭ実行制御Ｆ（資材）
     SELECT   CTLDCMSZ  ASSIGN    TO       DA-01-S-CTLDCMSZ
                        FILE      STATUS   IS  SZI-ST.
*ＤＣＭ実行制御Ｆ（植物）
     SELECT   CTLDCMSB  ASSIGN    TO       DA-01-S-CTLDCMSB
                        FILE      STATUS   IS  SKB-ST.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-ST.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*ＤＣＭ実行制御Ｆ（資材）
 FD  CTLDCMSZ          BLOCK     CONTAINS  255  RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      CTLDCMSZ  OF   XFDLIB
                       JOINING   SZI       AS   PREFIX.
*ＤＣＭ実行制御Ｆ（植物）
 FD  CTLDCMSB          BLOCK     CONTAINS  255  RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      CTLDCMSB  OF   XFDLIB
                       JOINING   SKB       AS   PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  SIZ-END-FLG             PIC  X(01)     VALUE  SPACE.
 01  SKB-END-FLG             PIC  X(01)     VALUE  SPACE.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
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
     03  SZI-ST              PIC  X(02).
     03  SKB-ST              PIC  X(02).
     03  PRT-ST              PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  ST-PG           PIC   X(08)  VALUE "DCM0050L".
         05  FILLER          PIC   X(11)  VALUE
                                          " START *** ".
     03  MSG-END.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "DCM0050L".
         05  FILLER          PIC   X(11)  VALUE
                                          " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "DCM0050L".
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
*
 01  SIZAI-DATA.
     03  SIZAI-SONZAI-CHK    PIC  X(01)    VALUE  SPACE.
     03  SIZAI-TOKKBN        PIC  X(03)    VALUE  SPACE.
     03  SIZAI-DTKBN         PIC  X(01)    VALUE  SPACE.
     03  SIZAI-KEKA-CD       PIC  X(03)    VALUE  SPACE.
 01  SYOKB-DATA.
     03  SYOKB-SONZAI-CHK    PIC  X(01)    VALUE  SPACE.
     03  SYOKB-TOKKBN        PIC  X(03)    VALUE  SPACE.
     03  SYOKB-DTKBN         PIC  X(01)    VALUE  SPACE.
     03  SYOKB-KEKA-CD       PIC  X(03)    VALUE  SPACE.
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER  TYPE YB-21.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "DCM0050L".
         05  FILLER          PIC  X(26)     VALUE  SPACE.
         05  FILLER          PIC  N(17)     VALUE
         NC"＜ＷＥＢＥＤＩ自動取込結果リスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(20)     VALUE  SPACE.
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
*
 01  HD01.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD021.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(28)   VALUE  SPACE.
         05  FILLER          PIC  N(20)   VALUE
         NC"ＷＥＢＥＤＩ自動取込結果は、下記の通りで".
         05  FILLER          PIC  N(10)   VALUE
         NC"す。　　　　　　　　".
*
 01  HD022.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(28)   VALUE  SPACE.
         05  FILLER          PIC  N(20)   VALUE
         NC"異常、警告の場合は、受信端末側情報を確認".
         05  FILLER          PIC  N(10)   VALUE
         NC"して下さい。　　　　".
*
 01  HD023.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(40)   VALUE  SPACE.
         05  FILLER          PIC  N(17)   VALUE
         NC"取引先情報：ＤＣＭホールディングス".
*
 01  MS01.
     03  FILLER              CHARACTER TYPE YA.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  MS01-KBNNM      PIC  N(04).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-KEKATL     PIC  N(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-KEKA       PIC  X(03).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-KEKANM     PIC  N(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-MSG1       PIC  N(20).
         05  MS01-MSG2       PIC  N(20).
*
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
 01  PARA-OUT-KEKA1         PIC   X(01).
 01  PARA-OUT-KEKA2         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-OUT-KEKA1
                                       PARA-OUT-KEKA2.
*---------------------------------------------------------------*
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CTLDCMSZ.
     MOVE      "CTLDCMSZ"   TO   AB-FILE.
     MOVE      SZI-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CTLDCMSB.
     MOVE      "CTLDCMSB"   TO   AB-FILE.
     MOVE      SKB-ST       TO   AB-STS.
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
     MOVE      PRT-ST       TO   AB-STS.
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
     PERFORM  MAIN-SEC.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     CTLDCMSZ  CTLDCMSB.
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
*ＤＣＭ受信制御Ｆ（資材）読込
     READ  CTLDCMSZ
           AT  END       MOVE  "1"      TO   SIZAI-SONZAI-CHK
                         MOVE  "DCM"    TO   SIZAI-TOKKBN
                         MOVE  "1"      TO   SIZAI-DTKBN
                         MOVE  "EEE"    TO   SIZAI-KEKA-CD
           NOT  AT  END  MOVE  SPACE    TO   SIZAI-SONZAI-CHK
                         MOVE  SZI-F01  TO   SIZAI-TOKKBN
                         MOVE  SZI-F02  TO   SIZAI-DTKBN
                         MOVE  SZI-F03  TO   SIZAI-KEKA-CD
     END-READ.
     READ  CTLDCMSB
           AT  END       MOVE  "1"      TO   SYOKB-SONZAI-CHK
                         MOVE  "DCM"    TO   SYOKB-TOKKBN
                         MOVE  "2"      TO   SYOKB-DTKBN
                         MOVE  "EEE"    TO   SYOKB-KEKA-CD
           NOT  AT  END  MOVE  SPACE    TO   SYOKB-SONZAI-CHK
                         MOVE  SKB-F01  TO   SYOKB-TOKKBN
                         MOVE  SKB-F02  TO   SYOKB-DTKBN
                         MOVE  SKB-F03  TO   SYOKB-KEKA-CD
     END-READ.
*
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
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
*資材関係状況印字
     MOVE     SPACE              TO   MS01.
     MOVE     NC"＜資材＞"       TO   MS01-KBNNM.
     MOVE     NC"結果："         TO   MS01-KEKATL.
*
     MOVE     SIZAI-KEKA-CD      TO   MS01-KEKA.
     EVALUATE SIZAI-KEKA-CD
         WHEN "000"
         MOVE NC"発注データの受信が成功しました。受信後の"
                      TO   MS01-MSG1
         MOVE NC"処理を確認して下さい　　　　　　　　　　"
                      TO   MS01-MSG2
*        MOVE "0"     TO   PARA-OUT-KEKA1
         WHEN "001"
         MOVE NC"発注データの受信で異常が発生しました。受"
                      TO   MS01-MSG1
         MOVE NC"信端末側で異常発生状況を確認して下さい。"
                      TO   MS01-MSG2
*        MOVE "1"     TO   PARA-OUT-KEKA1
         WHEN "002"
         MOVE NC"発注データの受信で警告が発生しました。受"
                      TO   MS01-MSG1
         MOVE NC"信端末側で警告発生状況を確認して下さい。"
                      TO   MS01-MSG2
*        MOVE "1"     TO   PARA-OUT-KEKA1
         WHEN "999"
         MOVE NC"受信＝０件です。本日のデータはありません"
                      TO   MS01-MSG1
         MOVE NC"。受信端末側の確認をお願い致します。　　"
                      TO   MS01-MSG2
*        MOVE "1"     TO   PARA-OUT-KEKA1
         WHEN "EEE"
         MOVE NC"受信制御ファイルの取得が出来ませんでした"
                      TO   MS01-MSG1
         MOVE NC"。受信端末側のログ確認をお願い致します。"
                      TO   MS01-MSG2
*        MOVE "1"     TO   PARA-OUT-KEKA1
         WHEN OTHER
         MOVE NC"自動受信処理の正常／異常の判定が出来ませ"
                      TO   MS01-MSG1
         MOVE NC"ん。ＮＡＶへ調査依頼して下さい。　　　　"
                      TO   MS01-MSG2
*        MOVE "1"     TO   PARA-OUT-KEKA1
     END-EVALUATE.
*
     WRITE    PRT-REC       FROM  MS01  AFTER  2.
 MAIN-020.
*植物関係状況印字
     MOVE     SPACE              TO   MS01.
     MOVE     NC"＜植物＞"       TO   MS01-KBNNM.
     MOVE     NC"結果："         TO   MS01-KEKATL.
*
     MOVE     SYOKB-KEKA-CD      TO   MS01-KEKA.
     EVALUATE SYOKB-KEKA-CD
         WHEN "000"
         MOVE NC"発注データの受信が成功しました。受信後の"
                      TO   MS01-MSG1
         MOVE NC"処理を確認して下さい　　　　　　　　　　"
                      TO   MS01-MSG2
*        MOVE "0"     TO   PARA-OUT-KEKA2
         WHEN "001"
         MOVE NC"発注データの受信で異常が発生しました。受"
                      TO   MS01-MSG1
         MOVE NC"信端末側で異常発生状況を確認して下さい。"
                      TO   MS01-MSG2
*        MOVE "1"     TO   PARA-OUT-KEKA2
         WHEN "002"
         MOVE NC"発注データの受信で警告が発生しました。受"
                      TO   MS01-MSG1
         MOVE NC"信端末側で警告発生状況を確認して下さい。"
                      TO   MS01-MSG2
*        MOVE "1"     TO   PARA-OUT-KEKA2
         WHEN "999"
         MOVE NC"受信＝０件です。本日のデータはありません"
                      TO   MS01-MSG1
         MOVE NC"。受信端末側の確認をお願い致します。　　"
                      TO   MS01-MSG2
*        MOVE "1"     TO   PARA-OUT-KEKA2
         WHEN "EEE"
         MOVE NC"受信制御ファイルの取得が出来ませんでした"
                      TO   MS01-MSG1
         MOVE NC"。受信端末側のログ確認をお願い致します。"
                      TO   MS01-MSG2
*        MOVE "1"     TO   PARA-OUT-KEKA2
         WHEN OTHER
         MOVE NC"自動受信処理の正常／異常の判定が出来ませ"
                      TO   MS01-MSG1
         MOVE NC"ん。ＮＡＶへ調査依頼して下さい。　　　　"
                      TO   MS01-MSG2
*        MOVE "1"     TO   PARA-OUT-KEKA2
     END-EVALUATE.
*
     WRITE    PRT-REC       FROM  MS01  AFTER  2.
*
 MAIN-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
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
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  2.
     WRITE    PRT-REC       FROM  HD021 AFTER  3.
     WRITE    PRT-REC       FROM  HD022 AFTER  2.
     WRITE    PRT-REC       FROM  HD023 AFTER  5.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  3.
*行カウントアップ
     MOVE     17                  TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC      SECTION.
*
     MOVE    "END-SEC"  TO      S-NAME.
*
     CLOSE    CTLDCMSZ  CTLDCMSB  PRTF.
*
     DISPLAY "DCM SIZAI CHK = " SIZAI-DATA UPON CONS.
     DISPLAY "DCM SYOKB CHK = " SYOKB-DATA UPON CONS.
**   DISPLAY "PARA-OUT-KEKA1 = " PARA-OUT-KEKA1 UPON CONS.
**   DISPLAY "PARA-OUT-KEKA2 = " PARA-OUT-KEKA2 UPON CONS.
*
     STOP     RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
