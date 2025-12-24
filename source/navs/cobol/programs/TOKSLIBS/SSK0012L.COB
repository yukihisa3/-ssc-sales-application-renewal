*************************************************************
* 顧客名　　　　: 株式会社サカタのタネ
* 業務名　　　　: ケーヨー伝票レス
* サブシステム名: ケーヨー伝票レス
* モジュール名　: 受領データ取込エラーリスト発行
* 作成日／作成者: 14/03/07  YOSHIDA.M
* 処理概要　　　: パラメタを受取り、バッチ_を生成し受領累積
*                データ（ＫＥＹ１）にスタートを掛け、取込エ
*                ラー区分１～１０の何れかが”１”のデータよ
*                り、取込エラーリストを出力する。
* 使用ＭＥＤ　　:
*************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSK0012L.
*
 ENVIRONMENT           DIVISION.
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
          YA      IS   CHR-2
          YA-22   IS   CHR-22
          YB      IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受領累積データ
     SELECT  KEIJYUF   ASSIGN    TO   KEIJYRL1
             ORGANIZATION        IS   INDEXED
             ACCESS MODE         IS   SEQUENTIAL
             FILE STATUS         IS   KEIJYUF-ST
             RECORD  KEY         IS   KEI-F01  KEI-F02
                                      KEI-F03  KEI-F04
                                      KEI-F05  KEI-F06
                                      KEI-F08  KEI-F10.
*プリントＦ
     SELECT  PRTF      ASSIGN         LP-04.
*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*受領累積データ
 FD  KEIJYUF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KEIJYRF   OF   XFDLIB
                       JOINING   KEI       AS   PREFIX.
*プリンター
 FD  PRTF
                       LABEL     RECORD    IS   OMITTED.
 01  PRT-REC           PIC  X(200).
*
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*共通領域
 01  PG-ID                       PIC  X(08)   VALUE  "SSK0012L".
*ステータス領域
 01  STATUS-AREA.
     03  KEIJYUF-ST              PIC  X(02).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME.
         05  WK-HH               PIC  9(02).
         05  WK-MN               PIC  9(02).
         05  WK-SS               PIC  9(02).
 01  DATE-AREA.
     03  WK-YS                   PIC  9(02).
     03  WK-DATE.
         05  WK-Y                PIC  9(02).
         05  WK-M                PIC  9(02).
         05  WK-D                PIC  9(02).
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                PIC  9(08).
*フラグ
 01  FLG-AREA.
     03  END-FLG                 PIC  X(03)  VALUE  SPACE.
*カウンタ領域
 01  COUNTER-AREA.
     03  CNT-PG                  PIC  9(04)  VALUE  ZERO.
     03  L-CNT                   PIC  9(03)  VALUE  ZERO.
*印刷定義エリア
 01  HD1-1.
     03  FILLER                  PIC  X(32)  VALUE  SPACE.
     03  FILLER                  PIC  N(15)  VALUE
         NC"＜受領データ取込エラーリスト＞"
         CHARACTER  TYPE  IS  CHR-22.
 01  HD1-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(08)  VALUE
                                 "SSK0012L".
     03  FILLER                  PIC  X(101) VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
         CHARACTER  TYPE  IS  CHR-2.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
         CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
         CHARACTER  TYPE  IS  CHR-2.
 01  HD2.
     03  FILLER                  PIC  X(112) VALUE  SPACE.
     03  HD2-01                  PIC  9(02).
     03  FILLER                  PIC  N(01)  VALUE  NC"："
         CHARACTER  TYPE  IS  CHR-2.
     03  HD2-02                  PIC  9(02).
     03  FILLER                  PIC  N(01)  VALUE  NC"："
         CHARACTER  TYPE  IS  CHR-2.
     03  HD2-03                  PIC  9(02).
 01  HD3.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"バッチＮＯ："
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD3-01                  PIC  9(08).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(01)  VALUE  "-".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD3-02                  PIC  9(04).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(01)  VALUE  "-".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD3-03                  PIC  9(08).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(07)  VALUE
                                 NC"エラー情報＝＞"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"_店舗Ｅ"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"_売上Ｅ"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  N(07)  VALUE
                                 NC"_変換ＴＢＬＥ"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"_名称Ｅ"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"_予備"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"_予備"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"_予備"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"_予備"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"_予備"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"_予備"
         CHARACTER  TYPE  IS  CHR-15.
 01  HD4.
     03  FILLER                  PIC  X(22)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"＜伝区＞"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(74)  VALUE  SPACE.
     03  FILLER                  PIC  N(15)  VALUE
         NC"＜－－－　エラー情報　－－－＞"
         CHARACTER  TYPE  IS  CHR-2.
 01  HD5.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"店舗ＣＤ"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"伝票番号"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"行"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"ケ"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"サ"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"納品予定日"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"検収日"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"ＪＡＮＣＤ"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"検品数量"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"原価単価"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"原価金額　"
         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  X(01)  VALUE  "!".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"_"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  "!".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"_"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  "!".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"_"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  "!".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"_"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  "!".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"_"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  "!".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"_"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  "!".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"_"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  "!".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"_"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  "!".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"_"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  "!".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"_"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  "!".
 01  SEN.
     03  FILLER                  PIC  X(50)  VALUE
         "==================================================".
     03  FILLER                  PIC  X(50)  VALUE
         "==================================================".
     03  FILLER                  PIC  X(36)  VALUE
         "====================================".
 01  SEN2.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "-!--!--!--!--!--!--!--!--!--!--!----".
 01  DT1.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-01                  PIC  9(05).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-02                  PIC  9(09).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-03                  PIC  Z9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-04                  PIC  X(02).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-05                  PIC  X(02).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-06                  PIC  9(08).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-07                  PIC  9(08).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-08                  PIC  X(13).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-09                  PIC  ---,--9.9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-10                  PIC  --,---,--9.99.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-11                  PIC  --,---,--9.99.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-12X                 PIC  X(01).
     03  DT1-12                  PIC  N(01)
         CHARACTER  TYPE  IS  CHR-2.
     03  DT1-13X                 PIC  X(01).
     03  DT1-13                  PIC  N(01)
         CHARACTER  TYPE  IS  CHR-2.
     03  DT1-14X                 PIC  X(01).
     03  DT1-14                  PIC  N(01)
         CHARACTER  TYPE  IS  CHR-2.
     03  DT1-15X                 PIC  X(01).
     03  DT1-15                  PIC  N(01)
         CHARACTER  TYPE  IS  CHR-2.
     03  DT1-16X                 PIC  X(01).
     03  DT1-16                  PIC  N(01)
         CHARACTER  TYPE  IS  CHR-2.
     03  DT1-17X                 PIC  X(01).
     03  DT1-17                  PIC  N(01)
         CHARACTER  TYPE  IS  CHR-2.
     03  DT1-18X                 PIC  X(01).
     03  DT1-18                  PIC  N(01)
         CHARACTER  TYPE  IS  CHR-2.
     03  DT1-19X                 PIC  X(01).
     03  DT1-19                  PIC  N(01)
         CHARACTER  TYPE  IS  CHR-2.
     03  DT1-20X                 PIC  X(01).
     03  DT1-20                  PIC  N(01)
         CHARACTER  TYPE  IS  CHR-2.
     03  DT1-21X                 PIC  X(01).
     03  DT1-21                  PIC  N(01)
         CHARACTER  TYPE  IS  CHR-2.
     03  DT1-22X                 PIC  X(01).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN                 PIC X(01).
 01  LINK-IN-YMD6                PIC 9(06).
 01  LINK-IN-YMD8                PIC 9(08).
 01  LINK-OUT-RET                PIC X(01).
 01  LINK-OUT-YMD                PIC 9(08).
*
 LINKAGE                     SECTION.
 01  LINK-AREA.
     03  LINK-JDATE              PIC 9(08).
     03  LINK-JTIME              PIC 9(04).
     03  LINK-JKAISEN            PIC 9(01).
     03  LINK-JYUSEN             PIC 9(01).
**************************************************************
 PROCEDURE             DIVISION  USING  LINK-AREA.
**************************************************************
*=============================================================
*       0.0       エラー処理
*=============================================================
 DECLARATIVES.
*受領累積データ
 KEIJYUF-ERR           SECTION.
     USE     AFTER     EXCEPTION      PROCEDURE     KEIJYUF.
     DISPLAY "**ABEND FILE NAME = KEIJYUF  **"         UPON CONS.
     DISPLAY "**ABEND STATUS    = " KEIJYUF-ST "   **" UPON CONS.
     ACCEPT  WK-TIME  FROM  TIME.
     DISPLAY "*** " PG-ID " ABEND (" WK-Y "/" WK-M "/"
             WK-D " " WK-HH ":" WK-MN ":" WK-SS ") ***"
             UPON  CONS.
     MOVE    "4000"         TO   PROGRAM-STATUS.
     STOP    RUN.
 END  DECLARATIVES.
*=============================================================
*       1.0       コントロール
*=============================================================
 CONTROL-SEC           SECTION.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG  =  "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 CONTROL-END.
     EXIT.
*=============================================================
*      2.0      初期処理
*=============================================================
 INIT-SEC              SECTION.
     MOVE   99   TO        L-CNT.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
     ACCEPT    WK-TIME          FROM   TIME.
     DISPLAY "*** " PG-ID " START (" WK-Y "." WK-M "." WK-D " "
             WK-HH "." WK-MN "." WK-SS ") ***"    UPON  CONS.
**
*ファイルのＯＰＥＮ
     OPEN    INPUT     KEIJYUF.
     OPEN    OUTPUT    PRTF.
*ワークの初期化
     INITIALIZE        FLG-AREA.
*受領累積データスタート
     MOVE    LINK-JDATE  TO  KEI-F01.
     MOVE    LINK-JTIME  TO  KEI-F02.
     MOVE    00000173    TO  KEI-F03.
     MOVE    ZERO        TO  KEI-F04  KEI-F05
                             KEI-F06  KEI-F08
                             KEI-F10.
     START   KEIJYUF  KEY  IS  >=  KEI-F01  KEI-F02
                                   KEI-F03  KEI-F04
                                   KEI-F05  KEI-F06
                                   KEI-F08  KEI-F10
             INVALID
             MOVE     "END"    TO   END-FLG
             DISPLAY
                NC"＃＃　対象データが存在しません！！　１　＃＃"
                                                     UPON  CONS
             GO                TO   INIT-END
     END-START.
*
*受領累積データ読込
     PERFORM KEIJYUF-READ-SEC.
     IF      END-FLG  =  "END"
             DISPLAY
                NC"＃＃　対象データが存在しません！！　２　＃＃"
                                                     UPON  CONS
     END-IF.
*
 INIT-END.
     EXIT.
*=============================================================
*       3.0       メイン処理
*=============================================================
 MAIN-SEC              SECTION.
*ヘッダ出力
     IF     L-CNT     > 54
            PERFORM     HEAD-SUB
     END-IF.
*明細出力
     PERFORM    BODY-SUB.
*受領累積データ読込
     PERFORM    KEIJYUF-READ-SEC.
*
 MAIN-END.
     EXIT.
*=============================================================
*       3.1.1     ＨＥＡＤ部編集処理
*=============================================================
 HEAD-SUB              SECTION.
*改頁判定
     IF      CNT-PG  >   ZERO
             MOVE    SPACE       TO   PRT-REC
             WRITE   PRT-REC     AFTER   PAGE
     END-IF.
     MOVE    ZERO    TO     L-CNT.
*頁カウント
     ADD     1                   TO   CNT-PG.
*システム日付
*年
     MOVE    SYS-DATE(1:4)       TO   HD1-01.
*月
     MOVE    SYS-DATE(5:2)       TO   HD1-02.
*日
     MOVE    SYS-DATE(7:2)       TO   HD1-03.
*頁
     MOVE    CNT-PG              TO   HD1-04.
*システム時刻
*時
     MOVE    WK-HH               TO   HD2-01.
*分
     MOVE    WK-MN               TO   HD2-02.
*秒
     MOVE    WK-SS               TO   HD2-03.
*受信日付
     MOVE    KEI-F01             TO   HD3-01.
*受信時間
     MOVE    KEI-F02             TO   HD3-02.
*取引先CD
     MOVE    KEI-F03             TO   HD3-03.
*ヘッダ行印字
     WRITE   PRT-REC   FROM   HD1-1 AFTER  2.
     WRITE   PRT-REC   FROM   HD1-2 AFTER  1.
     WRITE   PRT-REC   FROM   HD2   AFTER  1.
     WRITE   PRT-REC   FROM   HD3   AFTER  1.
     WRITE   PRT-REC   FROM   SEN   AFTER  1.
     WRITE   PRT-REC   FROM   HD4   AFTER  1.
     WRITE   PRT-REC   FROM   HD5   AFTER  1.
     WRITE   PRT-REC   FROM   SEN   AFTER  1.
     MOVE   11      TO      L-CNT.
*
 HEAD-SUB-END.
     EXIT.
*=============================================================
*       3.1.2     ＢＯＤＹ部  転送処理
*=============================================================
 BODY-SUB            SECTION.
*改頁チェック
     IF    L-CNT     >  54
           PERFORM   HEAD-SUB
     END-IF.
*明細行初期化
     INITIALIZE                              DT1.
     MOVE  SPACE                  TO         DT1.
*店舗ＣＤ
     MOVE  KEI-F04                TO         DT1-01.
*伝票番号
     MOVE  KEI-F05                TO         DT1-02.
*行
     MOVE  KEI-F06                TO         DT1-03.
*ケーヨー伝票区分
     MOVE  KEI-F07                TO         DT1-04.
*サカタ伝票区分
     MOVE  KEI-F08                TO         DT1-05.
*納品予定日
     MOVE  KEI-F09                TO         DT1-06.
*検収日
     MOVE  KEI-F10                TO         DT1-07.
*ＪＡＮＣＤ
     MOVE  KEI-F11                TO         DT1-08.
*検品数量
     MOVE  KEI-F12                TO         DT1-09.
*原価単価
     MOVE  KEI-F13                TO         DT1-10.
*原価金額
     MOVE  KEI-F14                TO         DT1-11.
*エラー区分１
     IF    KEI-F83  =  1
           MOVE NC"×"            TO         DT1-12
     ELSE
           MOVE  SPACE            TO         DT1-12
     END-IF.
     MOVE  "!"                    TO         DT1-12X.
*エラー区分２
     IF    KEI-F84  =  1
           MOVE NC"×"            TO         DT1-13
     ELSE
           MOVE  SPACE            TO         DT1-13
     END-IF.
     MOVE  "!"                    TO         DT1-13X.
*エラー区分３
     IF    KEI-F85  =  1
           MOVE NC"×"            TO         DT1-14
     ELSE
           MOVE  SPACE            TO         DT1-14
     END-IF.
     MOVE  "!"                    TO         DT1-14X.
*エラー区分４
     IF    KEI-F86  =  1
           MOVE NC"×"            TO         DT1-15
     ELSE
           MOVE  SPACE            TO         DT1-15
     END-IF.
     MOVE  "!"                    TO         DT1-15X.
*エラー区分５
     IF    KEI-F87  =  1
           MOVE NC"×"            TO         DT1-16
     ELSE
           MOVE  SPACE            TO         DT1-16
     END-IF.
     MOVE  "!"                    TO         DT1-16X.
*エラー区分６
     IF    KEI-F88  =  1
           MOVE NC"×"            TO         DT1-17
     ELSE
           MOVE  SPACE            TO         DT1-17
     END-IF.
     MOVE  "!"                    TO         DT1-17X.
*エラー区分７
     IF    KEI-F89  =  1
           MOVE NC"×"            TO         DT1-18
     ELSE
           MOVE  SPACE            TO         DT1-18
     END-IF.
     MOVE  "!"                    TO         DT1-18X.
*エラー区分８
     IF    KEI-F90  =  1
           MOVE NC"×"            TO         DT1-19
     ELSE
           MOVE  SPACE            TO         DT1-19
     END-IF.
     MOVE  "!"                    TO         DT1-19X.
*エラー区分９
     IF    KEI-F91  =  1
           MOVE NC"×"            TO         DT1-20
     ELSE
           MOVE  SPACE            TO         DT1-20
     END-IF.
     MOVE  "!"                    TO         DT1-20X.
*エラー区分１０
     IF    KEI-F92  =  1
           MOVE NC"×"            TO         DT1-21
     ELSE
           MOVE  SPACE            TO         DT1-21
     END-IF.
     MOVE  "!"                    TO         DT1-21X.
     MOVE  "!"                    TO         DT1-22X.
*明細行印字
     WRITE   PRT-REC   FROM    DT1    AFTER   1.
     WRITE   PRT-REC   FROM    SEN2   AFTER   1.
*明細行カウント
     ADD     2                    TO        L-CNT.
*
 BODY-SUB-END.
     EXIT.
*=============================================================
*       3.3       受領累積データ　ＲＥＡＤ
*=============================================================
 KEIJYUF-READ-SEC        SECTION.
*
     READ    KEIJYUF
             AT  END
             MOVE    "END"       TO   END-FLG
             GO                  TO   KEIJYUF-READ-EXIT
     END-READ.
*
     IF      KEI-F01  =  LINK-JDATE
     AND     KEI-F02  =  LINK-JTIME
     AND     KEI-F03  =  00000173
        IF   KEI-F83  =  SPACE
        AND  KEI-F84  =  SPACE
        AND  KEI-F85  =  SPACE
        AND  KEI-F86  =  SPACE
        AND  KEI-F87  =  SPACE
        AND  KEI-F88  =  SPACE
        AND  KEI-F89  =  SPACE
        AND  KEI-F90  =  SPACE
        AND  KEI-F91  =  SPACE
        AND  KEI-F92  =  SPACE
             GO                  TO   KEIJYUF-READ-SEC
        ELSE
             CONTINUE
        END-IF
     ELSE
             MOVE    "END"       TO   END-FLG
             GO                  TO   KEIJYUF-READ-EXIT
     END-IF.

*
 KEIJYUF-READ-EXIT.
     EXIT.
*=============================================================
*       4.0       終了処理
*=============================================================
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             KEIJYUF
                       PRTF.
**
     DISPLAY "*** " "出力枚数" " = " CNT-PG " "
             "ページ" " ***"    UPON  CONS.
**
     ACCEPT  WK-TIME             FROM      TIME.
     DISPLAY "*** " PG-ID " END   (" WK-Y "." WK-M "." WK-D " "
              WK-HH "." WK-MN "." WK-SS ") ***"    UPON  CONS.
 END-END.
     EXIT.
*****************<<  SSK0012L END PROGRAM  >>******************
