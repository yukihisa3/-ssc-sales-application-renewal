*************************************************************
* 顧客名　　　　: 株式会社サカタのタネ
* 業務名　　　　: ケーヨー伝票レス
* サブシステム名: ケーヨー伝票レス
* モジュール名　: ケーヨー受領データ受信件数リスト発行
* 作成日／作成者: 14/03/05  YOSHIDA.M
* 処理概要　　　: バッチ_を受取り、バッチ_で受領受信状況Ｆ
*                にスタートを掛け、ケーヨー受領データ受信件数
*                リストを出力する。
* 使用ＭＥＤ　　:
*************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSK0013L.
*
 ENVIRONMENT           DIVISION.
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
          YA      IS   CHR-2
          YB-22   IS   CHR-22
          YB      IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受領受信状況Ｆ
     SELECT  KEIJYOF   ASSIGN    TO   KEIJYOL1
             ORGANIZATION        IS   INDEXED
             ACCESS MODE         IS   SEQUENTIAL
             FILE STATUS         IS   KEIJYOF-ST
             RECORD  KEY         IS   KEI-F01
                                      KEI-F02
                                      KEI-F03
                                      KEI-F04.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO   TOKMS2
             ORGANIZATION        IS   INDEXED
             ACCESS MODE         IS   RANDOM
             FILE STATUS         IS   HTOKMS-ST
             RECORD  KEY         IS   TOK-F01.
*プリントＦ
     SELECT  PRTF      ASSIGN         LP-04.
*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*受領受信状況Ｆ
 FD  KEIJYOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KEIJYOF   OF   XFDLIB
                       JOINING   KEI       AS   PREFIX.
*取引先マスタ
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
*プリンター
 FD  PRTF
                       LABEL     RECORD    IS   OMITTED.
 01  PRT-REC           PIC  X(200).
*
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*共通領域
 01  PG-ID                       PIC  X(08)   VALUE  "SSK0013L".
*ステータス領域
 01  STATUS-AREA.
     03  KEIJYOF-ST              PIC  X(02).
     03  HTOKMS-ST               PIC  X(02).
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
*合計領域
 01  GOKEI-AREA.
     03  WK-DTCNT                PIC  9(11)  VALUE  ZERO.
     03  WK-RUICNT               PIC  9(11)  VALUE  ZERO.
     03  WK-KEICNT               PIC  9(11)  VALUE  ZERO.
*印刷定義エリア
 01  HD1-1.
     03  FILLER                  PIC  X(28)  VALUE  SPACE.
     03  FILLER                  PIC  N(20)  VALUE
         NC"　＜　ケーヨー受領　　受信件数リスト　＞"
         CHARACTER  TYPE  IS  CHR-22.
 01  HD1-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(08)  VALUE
                                 "SSK0013L".
 01  HD2.
     03  FILLER                  PIC  X(48)  VALUE  SPACE.
     03  HD2-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
         CHARACTER  TYPE  IS  CHR-2.
     03  HD2-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
         CHARACTER  TYPE  IS  CHR-2.
     03  HD2-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD2-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
         CHARACTER  TYPE  IS  CHR-2.
 01  HD3.
     03  FILLER                  PIC  X(50)  VALUE  SPACE.
     03  HD3-01                  PIC  9(02).
     03  FILLER                  PIC  N(01)  VALUE  NC"："
         CHARACTER  TYPE  IS  CHR-2.
     03  HD3-02                  PIC  9(02).
     03  FILLER                  PIC  N(01)  VALUE  NC"："
         CHARACTER  TYPE  IS  CHR-2.
     03  HD3-03                  PIC  9(02).
 01  HD4.
     03  FILLER                  PIC  X(34)  VALUE  SPACE.
     03  FILLER                  PIC  N(07)  VALUE
                                 NC"受信バッチ_："
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD4-01                  PIC  9(08).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(01)  VALUE  "-".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD4-02                  PIC  9(04).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(01)  VALUE  "-".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD4-03                  PIC  9(08).
 01  HD5.
     03  FILLER                  PIC  X(34)  VALUE  SPACE.
     03  FILLER                  PIC  N(07)  VALUE
                                 NC"受信取引先　："
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD5-01                  PIC  N(15)
         CHARACTER  TYPE  IS  CHR-2.
 01  HD6.
     03  FILLER                  PIC  X(26)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"ケーヨー伝区"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"サカタ伝区"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"データ件数"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"返品累積件数"
         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  N(06)  VALUE
                                 NC"返品計上件数"
         CHARACTER  TYPE  IS  CHR-2.
 01  SEN.
     03  FILLER                  PIC  X(26)  VALUE  SPACE.
     03  FILLER                  PIC  X(50)  VALUE
         "==================================================".
     03  FILLER                  PIC  X(18)  VALUE
         "==================".
 01  DT1.
     03  FILLER                  PIC  X(31)  VALUE  SPACE.
     03  DT1-01                  PIC  X(02).
     03  FILLER                  PIC  X(11)  VALUE  SPACE.
     03  DT1-02                  PIC  X(02).
     03  FILLER                  PIC  X(09)  VALUE  SPACE.
     03  DT1-03                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-04                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-05                  PIC  Z,ZZZ,ZZ9.
 01  DT2                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(46)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE  NC"合計：".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT2-01                  PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  DT2-02                  PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  DT2-03                  PIC  ZZZ,ZZZ,ZZ9.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN                 PIC X(01).
 01  LINK-IN-YMD6                PIC 9(06).
 01  LINK-IN-YMD8                PIC 9(08).
 01  LINK-OUT-RET                PIC X(01).
 01  LINK-OUT-YMD                PIC 9(08).
*
 LINKAGE                     SECTION.
 01  LINK-JDATE                  PIC 9(08).
 01  LINK-JTIME                  PIC 9(04).
 01  LINK-JTOKCD                 PIC 9(08).
**************************************************************
 PROCEDURE             DIVISION  USING  LINK-JDATE
                                        LINK-JTIME
                                        LINK-JTOKCD.
**************************************************************
*=============================================================
*       0.0       エラー処理
*=============================================================
 DECLARATIVES.
*受領受信状況Ｆ
 KEIJYOF-ERR           SECTION.
     USE     AFTER     EXCEPTION      PROCEDURE     KEIJYOF.
     DISPLAY "**ABEND FILE NAME = KEIJYOF  **"         UPON CONS.
     DISPLAY "**ABEND STATUS    = " KEIJYOF-ST "   **" UPON CONS.
     ACCEPT  WK-TIME  FROM  TIME.
     DISPLAY "*** " PG-ID " ABEND (" WK-Y "/" WK-M "/"
             WK-D " " WK-HH ":" WK-MN ":" WK-SS ") ***"
             UPON  CONS.
     MOVE    "4000"         TO   PROGRAM-STATUS.
     STOP    RUN.
*取引先マスタ
 HTOKMS-ERR            SECTION.
     USE     AFTER     EXCEPTION      PROCEDURE     HTOKMS.
     DISPLAY "**ABEND FILE NAME = HTOKMS   **"         UPON CONS.
     DISPLAY "**ABEND STATUS    = " HTOKMS-ST "    **" UPON CONS.
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
     OPEN    INPUT     KEIJYOF HTOKMS.
     OPEN    OUTPUT    PRTF.
*ワークの初期化
     INITIALIZE        FLG-AREA GOKEI-AREA.
*受領受信状況Ｆスタート
     MOVE    LINK-JDATE  TO  KEI-F01.
     MOVE    LINK-JTIME  TO  KEI-F02.
     MOVE    LINK-JTOKCD TO  KEI-F03.
     MOVE    ZERO        TO  KEI-F04.
     START   KEIJYOF  KEY  IS  >=  KEI-F01  KEI-F02
                                   KEI-F03  KEI-F04
             INVALID
             MOVE     "END"    TO   END-FLG
             DISPLAY
                NC"＃＃　対象データが存在しません！！　１　＃＃"
                                                     UPON  CONS
             GO                TO   INIT-END
     END-START.
*
*受領受信状況Ｆ読込
     PERFORM KEIJYOF-READ-SEC.
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
     IF     L-CNT     > 51
            PERFORM     HEAD-SUB
     END-IF.
*明細出力
     PERFORM    BODY-SUB.
*受領受信状況Ｆ読込
     PERFORM    KEIJYOF-READ-SEC.
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
     MOVE    SYS-DATE(1:4)       TO   HD2-01.
*月
     MOVE    SYS-DATE(5:2)       TO   HD2-02.
*日
     MOVE    SYS-DATE(7:2)       TO   HD2-03.
*頁
     MOVE    CNT-PG              TO   HD2-04.
*システム時刻
*時
     MOVE    WK-HH               TO   HD3-01.
*分
     MOVE    WK-MN               TO   HD3-02.
*秒
     MOVE    WK-SS               TO   HD3-03.
*受信日付
     MOVE    KEI-F01             TO   HD4-01.
*受信時間
     MOVE    KEI-F02             TO   HD4-02.
*取引先CD
     MOVE    KEI-F03             TO   HD4-03  TOK-F01.
*取引先名
     PERFORM TOK-READ-SEC.
     MOVE    TOK-F03             TO   HD5-01.
*ヘッダ行印字
     WRITE   PRT-REC   FROM   HD1-1 AFTER  2.
     WRITE   PRT-REC   FROM   HD1-2 AFTER  1.
     WRITE   PRT-REC   FROM   HD2   AFTER  2.
     WRITE   PRT-REC   FROM   HD3   AFTER  1.
     WRITE   PRT-REC   FROM   SEN   AFTER  2.
     WRITE   PRT-REC   FROM   HD4   AFTER  2.
     WRITE   PRT-REC   FROM   HD5   AFTER  2.
     WRITE   PRT-REC   FROM   SEN   AFTER  2.
     WRITE   PRT-REC   FROM   HD6   AFTER  2.
     MOVE    SPACE            TO    PRT-REC.
     WRITE   PRT-REC   AFTER  1.
     MOVE   19      TO      L-CNT.
*
 HEAD-SUB-END.
     EXIT.
*=============================================================
*       3.1.2     ＢＯＤＹ部  転送処理
*=============================================================
 BODY-SUB            SECTION.
*改頁チェック
     IF    L-CNT     >  51
           PERFORM   HEAD-SUB
     END-IF.
*明細行初期化
     INITIALIZE                              DT1.
     MOVE  SPACE                  TO         DT1.
*ケーヨー伝区
     MOVE  KEI-F04                TO         DT1-01.
*サカタ伝区
     MOVE  KEI-F05                TO         DT1-02.
*データ件数
     MOVE  KEI-F06                TO         DT1-03.
*返品累積件数
     MOVE  KEI-F07                TO         DT1-04.
*返品計上件数
     MOVE  KEI-F08                TO         DT1-05.
*明細行印字
     WRITE   PRT-REC   FROM    DT1    AFTER   1.
*明細行カウント
     ADD     1                    TO        L-CNT.
*合計カウント
     ADD     KEI-F06              TO        WK-DTCNT.
     ADD     KEI-F07              TO        WK-RUICNT.
     ADD     KEI-F08              TO        WK-KEICNT.
*
 BODY-SUB-END.
     EXIT.
*=============================================================
*       3.1.3     合計部  転送処理
*=============================================================
 GOKEI-SUB           SECTION.
*データ件数
     MOVE  WK-DTCNT               TO         DT2-01.
*返品累積件数
     MOVE  WK-RUICNT              TO         DT2-02.
*返品計上件数
     MOVE  WK-KEICNT              TO         DT2-03.
*合計行印字
     WRITE   PRT-REC   FROM    DT2    AFTER   2.
*合計行カウント
     ADD     2                    TO        L-CNT.
*
 GOKEI-SUB-END.
     EXIT.
*=============================================================
*       3.3       受領受信状況Ｆ　ＲＥＡＤ
*=============================================================
 KEIJYOF-READ-SEC        SECTION.
*
     READ    KEIJYOF
             AT  END
             MOVE    "END"       TO   END-FLG
             GO                  TO   KEIJYOF-READ-EXIT
     END-READ.
*
     IF      KEI-F01  =  LINK-JDATE
     AND     KEI-F02  =  LINK-JTIME
     AND     KEI-F03  =  LINK-JTOKCD
             CONTINUE
     ELSE
             MOVE    "END"       TO   END-FLG
             GO                  TO   KEIJYOF-READ-EXIT
     END-IF.

*
 KEIJYOF-READ-EXIT.
     EXIT.
*=============================================================
*       3.4       取引先マスタ　ＲＥＡＤ
*=============================================================
 TOK-READ-SEC        SECTION.
*
     READ    HTOKMS  INVALID
             MOVE    SPACE       TO   TOK-F03
     END-READ.
*
 TOK-READ-EXIT.
     EXIT.
*=============================================================
*       4.0       終了処理
*=============================================================
 END-SEC               SECTION.
*合計出力
     IF  CNT-PG  >  0
         PERFORM       GOKEI-SUB
     END-IF.
*ファイル ＣＬＯＳＥ
     CLOSE             KEIJYOF
                       HTOKMS
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
*****************<<  SSK0013L END PROGRAM  >>******************
