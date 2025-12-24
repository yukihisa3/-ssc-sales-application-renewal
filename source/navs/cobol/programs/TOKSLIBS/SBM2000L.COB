****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　イオン流通ＢＭＳ　　　　　　　　　*
*    業務名　　　　　　　：　イオン流通ＢＭＳ                  *
*    モジュール名　　　　：　請求送信データ確認リスト　　　    *
*    作成日／更新日　　　：　13/05/17                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　イオン請求送信Ｆを読込み、会社毎に*
*                            送信レコード件数、請求金額を集計し*
*                            リスト出力する。                  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBM2000L.
 AUTHOR.               NAV.
 DATE-WRITTEN.         12/11/22.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   YA
     YB           IS   YB
     YA-22        IS   YA-22
     YB-21        IS   YB-21
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*イオン請求送信ファイル
     SELECT  SEKIONF      ASSIGN    TO     DA-01-S-SEKIONF
                          FILE      STATUS SEK-ST.
*条件ファイル
     SELECT  HJYOKEN      ASSIGN    TO     DA-01-VI-JYOKEN1
                          ORGANIZATION     INDEXED
                          ACCESS    MODE   RANDOM
                          RECORD    KEY    JYO-F01  JYO-F02
                          FILE      STATUS JYO-ST.
*プリント定義ファイル
     SELECT  PRTFILE      ASSIGN    TO     LP-04-PRTF
                          FILE      STATUS PRT-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = イオン請求送信ファイル                             *
****************************************************************
 FD  SEKIONF
                       BLOCK     CONTAINS  3    RECORDS
                       LABEL     RECORD    IS   STANDARD.
 01  SEK-REC.
     03  SEK-F01       PIC  X(01).
     03  SEK-F02       PIC  X(1199).
****************************************************************
*  FILE= 条件ファイル                                          *
****************************************************************
 FD  HJYOKEN           BLOCK     CONTAINS  6    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
****************************************************************
*  FILE=プリントファイル                                       *
****************************************************************
 FD  PRTFILE
                       LABEL     RECORD    IS   OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SEK-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  SEK-ERR           PIC N(15) VALUE
         NC"イオン請求送信データエラー".
     03  JYO-ERR           PIC N(15) VALUE
         NC"条件ファイルエラー".
     03  PRT-ERR           PIC N(15) VALUE
         NC"プリントファイルエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
***  ﾜｰｸｴﾘｱ
 01  WK-AREA.
     03  END-FLG           PIC X(03) VALUE     SPACE.
     03  P-CNT             PIC 9(05) VALUE     ZERO.
     03  L-CNT             PIC 9(05) VALUE     ZERO.
     03  SYSDATE           PIC 9(06) VALUE     ZERO.
     03  IX                PIC 9(02) VALUE     ZERO.
     03  HJYOKEN-INV-FLG   PIC X(03) VALUE     SPACE.
     03  READ-CNT          PIC 9(07) VALUE     ZERO.
     03  WK-KAISYACD       PIC X(04) VALUE     SPACE.
     03  WK-MEISAI-KIN     PIC S9(12) VALUE    ZERO.
     03  WK-GOKEI-KIN      PIC S9(12) VALUE    ZERO.
     03  WK-MEISAI         PIC S9(12) VALUE    ZERO.
***  日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1        PIC  9(02).
     03  WK-DATE8-YY2        PIC  9(06).
 01  WK-DATE8-R         REDEFINES  WK-DATE8.
     03  WK-YYYY             PIC  9(04).
     03  WK-MM               PIC  9(02).
     03  WK-DD               PIC  9(02).
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
*レコードカイントエリア
 01  WK-COUNT-AREA1.
     03  WK-B-CNT1           PIC  9(05)  VALUE  ZERO.
     03  WK-C-CNT1           PIC  9(05)  VALUE  ZERO.
     03  WK-D-CNT1           PIC  9(05)  VALUE  ZERO.
 01  WK-COUNT-AREA2.
     03  WK-A-CNT2           PIC  9(05)  VALUE  ZERO.
     03  WK-B-CNT2           PIC  9(05)  VALUE  ZERO.
     03  WK-C-CNT2           PIC  9(05)  VALUE  ZERO.
     03  WK-D-CNT2           PIC  9(05)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*ヘッダ１
 01  HD1.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-01                  PIC  X(08).
     03  FILLER                  PIC  X(15)  VALUE  SPACE.
     03  FILLER                  PIC  N(19)  VALUE
         NC"＜流通ＢＭＳ請求送信データ確認リスト＞"
                                 CHARACTER  TYPE  IS  YA-22.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  HD1-02                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  YA.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  YA.
     03  HD1-04                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  YA.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-05                  PIC  Z9.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  HD1-06                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  HD1-07                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-08                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  YA.
*ヘッダ２
 01  HD2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(07)  VALUE
                                 NC"データ作成日："
                                 CHARACTER   TYPE   IS  YA.
     03  HD2-01                  PIC  9(08).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD2-02                  PIC  Z9.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  HD2-03                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  HD2-04                  PIC  99.
*ヘッダ３
 01  HD3.
     03  FILLER                  PIC  X(56)  VALUE  SPACE.
     03  FILLER                  PIC  N(16)  VALUE
         NC"＜－レ　コ　ー　ド　情　報－－＞"
                                 CHARACTER   TYPE   IS  YA.
*ヘッダ４
 01  HD4                         CHARACTER   TYPE   IS  YA.
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"会社情報".
     03  FILLER                  PIC  X(40)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"請求".
     03  FILLER                  PIC  X(03)  VALUE  "LST".
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"請求情報".
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"取引情報".
     03  FILLER                  PIC  X(11)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"請求金額".
*線＝
 01  SEN1.
     03  FILLER                  PIC  X(136) VALUE  ALL "=".
*線－
 01  SEN2.
     03  FILLER                  PIC  X(136) VALUE  ALL "-".
*明細
 01  MS1                         CHARACTER  TYPE  IS  YA.
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  MS1-01                  PIC  X(04).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  MS1-02                  PIC  N(20).
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  MS1-03                  PIC  ZZ,ZZ9.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  MS1-04                  PIC  ZZ,ZZ9.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  MS1-05                  PIC  ZZ,ZZ9.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  MS1-06                  PIC  ---,---,---,--9.
*合計１
 01  GK1                         CHARACTER  TYPE  IS  YA.
     03  FILLER                  PIC  X(73)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
         NC"＜請求合計＞".
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  GK1-01                  PIC  ---,---,---,--9.
*合計２
 01  GK2                         CHARACTER  TYPE  IS  YA.
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
         NC"＜レコード情報＞".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(08)  VALUE  "FILEﾍｯﾀﾞ".
     03  FILLER                  PIC  N(03)  VALUE  NC"件数＝".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  GK2-01                  PIC  ZZ,ZZ9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE  NC"請求".
     03  FILLER                  PIC  X(03)  VALUE  "LST".
     03  FILLER                  PIC  N(03)  VALUE  NC"件数＝".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  GK2-02                  PIC  ZZ,ZZ9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(07)  VALUE
         NC"請求情報件数＝".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  GK2-03                  PIC  ZZ,ZZ9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(07)  VALUE
         NC"取引情報件数＝".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  GK2-04                  PIC  ZZ,ZZ9.
*流通ＢＭＳ請求ファイルコピー句定義
     COPY  BMSSEKF  OF  XFDLIB  JOINING  SWK  PREFIX.
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 SEK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SEKIONF.
     MOVE        SEK-ST    TO        E-ST.
     MOVE       "SEKIONF " TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SEK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃イオン請求送信データ　　異常＃" UPON CONS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE       "JYOKEN1 " TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃条件ファイル　　　　　　異常＃" UPON CONS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     MOVE        PRT-ST    TO        E-ST.
     MOVE        "PRTFILE " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     PRT-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃請求送信データ確認リスト異常＃" UPON CONS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     DISPLAY NC"＃請求送信データ確認リスト開始＃" UPON CONS.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM   END-SEC.
     DISPLAY NC"＃請求送信データ確認リスト終了＃" UPON CONS.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT  SEKIONF  HJYOKEN.
     OPEN      OUTPUT PRTFILE.
*
*システム日付・時刻の取得
     ACCEPT   SYSDATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYSDATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-DATE8.
*    起動時の時刻で更新する。
     ACCEPT   SYS-TIME    FROM  TIME.
     MOVE     SYS-TIME            TO   WK-SYS-TIME.
*行カウンターを初期セット
     MOVE     99          TO      L-CNT.
*イオン請求送信デー読込
     PERFORM  SEKIONF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE  SEK-F01
         WHEN  "A"  ADD  1        TO  WK-A-CNT2
                    MOVE SEK-REC  TO  SWK-F10
         WHEN  "B"  MOVE SEK-REC  TO  SWK-F20
                    IF   WK-B-CNT2  >  ZERO
                         IF  SWK-F206  NOT = WK-KAISYACD
                             PERFORM  MEISAI-WT-SEC
                         END-IF
                    END-IF
                    MOVE ZERO     TO  WK-COUNT-AREA1
                    MOVE ZERO     TO  WK-MEISAI-KIN
                    MOVE SWK-F206 TO  WK-KAISYACD
                    ADD  1        TO  WK-B-CNT1  WK-B-CNT2
         WHEN  "C"  MOVE SEK-REC  TO  SWK-F30
                    ADD  1        TO  WK-C-CNT1  WK-C-CNT2
         WHEN  "D"  MOVE SEK-REC  TO  SWK-F40
                    IF   SWK-F417  =  "-"
                         COMPUTE WK-MEISAI = SWK-F416 * -1
                    ELSE
                         COMPUTE WK-MEISAI = SWK-F416
                    END-IF
                    ADD  WK-MEISAI TO WK-MEISAI-KIN WK-GOKEI-KIN
                    ADD  1        TO  WK-D-CNT1  WK-D-CNT2
     END-EVALUATE.
*次レコード読み込み
     PERFORM  SEKIONF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    イオン取引先受信ワーク読込                      ALL       *
****************************************************************
 SEKIONF-READ-SEC      SECTION.
     MOVE "SEKIONF-READ-SEC" TO   S-NAME.
*
     READ  SEKIONF  AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   SEKIONF-READ-EXIT
     END-READ.
*
     ADD   1                 TO   READ-CNT.
*
     IF  READ-CNT(5:3) = "000" OR "500"
         DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
 SEKIONF-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル読込                                ALL       *
****************************************************************
 HJYOKEN-READ-SEC       SECTION.
     MOVE "HJYOKEN-READ-SEC"  TO   S-NAME.
*
     READ  HJYOKEN
           INVALID      MOVE  "INV"   TO   HJYOKEN-INV-FLG
           NOT  INVALID MOVE  SPACE   TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*    ヘッダ印字処理                                  ALL       *
****************************************************************
 HEAD-WT-SEC           SECTION.
     MOVE "HEAD-WT-SEC"      TO   S-NAME.
*改頁チェック
     IF  P-CNT  >  ZERO
         MOVE   SPACE        TO   PRT-REC
         WRITE  PRT-REC      AFTER   PAGE
     END-IF.
*ヘッダを出力する。
*    ＰＧＩＤ
     MOVE      "SBM2000L"             TO   HD1-01.
*    システム日付
     MOVE      WK-YYYY                TO   HD1-02.
     MOVE      WK-MM                  TO   HD1-03.
     MOVE      WK-DD                  TO   HD1-04.
*    時刻
     MOVE      SYS-HH                 TO   HD1-05.
     MOVE      SYS-MN                 TO   HD1-06.
     MOVE      SYS-SS                 TO   HD1-07.
*    頁
     ADD       1                      TO   P-CNT.
     MOVE      P-CNT                  TO   HD1-08.
*    受信バッチ番号（日付）
     MOVE      SWK-F112(1:8)          TO   HD2-01.
     MOVE      SWK-F112(9:2)          TO   HD2-02.
     MOVE      SWK-F112(11:2)         TO   HD2-03.
     MOVE      SWK-F112(13:2)         TO   HD2-04.
 MAIN040.
*ヘッダ部出力
     WRITE    PRT-REC   FROM          HD1  AFTER  4.
     WRITE    PRT-REC   FROM          HD2  AFTER  2.
     WRITE    PRT-REC   FROM          SEN1 AFTER  2.
     WRITE    PRT-REC   FROM          HD3  AFTER  1.
     WRITE    PRT-REC   FROM          HD4  AFTER  1.
     WRITE    PRT-REC   FROM          SEN1 AFTER  1.
*行カウンター
     MOVE     11                      TO   L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*    明細印字処理　                                  ALL       *
****************************************************************
 MEISAI-WT-SEC         SECTION.
     MOVE "MEISAI-WT-SEC"    TO   S-NAME.
*ヘッダー出力チェック
     IF  L-CNT  >  61
         PERFORM  HEAD-WT-SEC
     END-IF.
*    会社ＣＤ
     MOVE      WK-KAISYACD            TO   MS1-01.
*    会社名取得
     MOVE      "76"                   TO   JYO-F01.
     MOVE      WK-KAISYACD            TO   JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG  = "INV"
              MOVE  ALL NC"＊"        TO   MS1-02
     ELSE
              MOVE  JYO-F03           TO   MS1-02
     END-IF.
*    請求ＬＳＴ件数
     MOVE     WK-B-CNT1               TO   MS1-03.
*    請求情報件数
     MOVE     WK-C-CNT1               TO   MS1-04.
*    取引情報件数
     MOVE     WK-D-CNT1               TO   MS1-05.
*    請求金額
     MOVE     WK-MEISAI-KIN           TO   MS1-06.
*明細出力
     WRITE    PRT-REC   FROM      MS1      AFTER  2.
     WRITE    PRT-REC   FROM      SEN2     AFTER  2.
*行カウンター
     MOVE     4                       TO   L-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*    合計印字処理　                                  ALL       *
****************************************************************
 GOKEI-WT-SEC          SECTION.
     MOVE "GOKEI-WT-SEC"     TO   S-NAME.
*ヘッダー出力チェック
     IF  L-CNT  >  61
         PERFORM  HEAD-WT-SEC
     END-IF.
*    請求金額チェック
     MOVE      WK-GOKEI-KIN           TO   GK1-01.
*    FILEﾍｯﾀﾞ件数
     MOVE      WK-A-CNT2              TO   GK2-01.
*    請求LST件数
     MOVE      WK-B-CNT2              TO   GK2-02.
*    請求情報件数
     MOVE      WK-C-CNT2              TO   GK2-03.
*    取引情報件数
     MOVE      WK-D-CNT2              TO   GK2-04.
*明細出力
     WRITE    PRT-REC   FROM      GK1      AFTER  2.
     WRITE    PRT-REC   FROM      GK2      AFTER  2.
*
 GOKEI-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*最終処理
     IF  WK-D-CNT1  >  ZERO
         PERFORM  MEISAI-WT-SEC
         PERFORM  GOKEI-WT-SEC
     END-IF.
*ファイルのＯＰＥＮ
     CLOSE     SEKIONF.
     CLOSE     HJYOKEN.
     CLOSE     PRTFILE.
*
     DISPLAY NC"＃入力件数　＝　" READ-CNT  UPON CONS.
     DISPLAY NC"＃出力頁　　＝　" P-CNT     UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  SBM2000L   END PROGRAM  >>******************
