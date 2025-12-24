****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　オンライン連携状況ファイル更新　  *
*    作成日／更新日　　　：　2012/10/05                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取った各パラメタより、売上伝票*
*                            データファイルから連携情報を取得し*
*                            オンライン連携状況Ｆを更新する。  *
*　　更新日／更新者　　　：　                                  *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：                                    *
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0100B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/10/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENLJ
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F112
                                            DEN-F48   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F03
                        FILE  STATUS   IS   DEN-ST.
*オンライン連携状況Ｆ
     SELECT  LNKONLF    ASSIGN    TO        DA-01-VI-LNKONLL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       LNK-F04  LNK-F01
                                            LNK-F02  LNK-F03
                                            LNK-F05
                        FILE  STATUS   IS   LNK-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    オンライン連携状況ファイル
******************************************************************
 FD  LNKONLF
                        LABEL     RECORD     IS  STANDARD.
     COPY     LNKONLF  OF        XFDLIB
              JOINING   LNK   AS  PREFIX.
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WRT-FLG                 PIC  X(03)     VALUE  SPACE.
 01  SET-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  NOT-CNT                 PIC  9(08)     VALUE  ZERO.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  DEN-ST              PIC  X(02).
     03  LNK-ST              PIC  X(02).
*集計カウント
 01  CNT-RNK                 PIC  9(07)     VALUE  ZERO.
 01  CNT-MIRNK               PIC  9(07)     VALUE  ZERO.
*退避エリア　
 01  WK-DEN.
     03  WK-DEN-F48          PIC  X(02)     VALUE  SPACE.
     03  WK-DEN-F112         PIC  9(08)     VALUE  ZERO.
 01  HEN-DEN.
     03  HEN-DEN-F48         PIC  X(02)     VALUE  SPACE.
     03  HEN-DEN-F112        PIC  9(08)     VALUE  ZERO.

***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMNSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).

***  セクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  URI-ERR           PIC  N(20)  VALUE
         NC"売上伝票ファイルエラー".
     03  LNK-ERR           PIC  N(20)  VALUE
         NC"オンライン連携状況ファイルエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBT0100B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0100B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " INPUT  = ".
         05  IN-CNT         PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " REWRITE= ".
         05  OUT-CNT        PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-NON.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " NONWRITE=".
         05  NON-CNT        PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*パラメタ定義
 LINKAGE                SECTION.
* 入力パラメタ
*    部門CD
 01  PARA-IN-BUMONCD        PIC   X(04).
*    担当者CD
 01  PARA-IN-TANCD          PIC   X(02).
*    抽出倉庫CD
 01  PARA-IN-SOUKO          PIC   X(02).
*    バッチ_（受信日）
 01  PARA-IN-ONL-JUSIN-HI   PIC   9(08).
*    バッチ_（受信時刻）
 01  PARA-IN-ONL-JUSIN-JI   PIC   9(04).
*    バッチ_（取引先）
 01  PARA-IN-ONL-JUSIN-TOR  PIC   9(08).
*    納品日（FROM）
 01  PARA-IN-ONL-NOUHIN-FR  PIC   9(08).
*    納品日（TO）
 01  PARA-IN-ONL-NOUHIN-TO  PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-IN-BUMONCD
                            PARA-IN-TANCD
                            PARA-IN-SOUKO
                            PARA-IN-ONL-JUSIN-HI
                            PARA-IN-ONL-JUSIN-JI
                            PARA-IN-ONL-JUSIN-TOR
                            PARA-IN-ONL-NOUHIN-FR
                            PARA-IN-ONL-NOUHIN-TO.
 DECLARATIVES.
 URI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 LNK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE LNKONLF.
     DISPLAY     LNK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     LNK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENF.
     OPEN     I-O       LNKONLF.
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE     TO        END-FLG.
     MOVE     ZERO      TO        RD-CNT    WRT-CNT  NOT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT  NON-CNT.
     MOVE     ZERO      TO        CNT-RNK   CNT-MIRNK.
*売上伝票ファイルスタート
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-IN-ONL-JUSIN-HI  TO   DEN-F46.
     MOVE     PARA-IN-ONL-JUSIN-JI  TO   DEN-F47.
     MOVE     PARA-IN-ONL-JUSIN-TOR TO   DEN-F01.
     MOVE     PARA-IN-SOUKO         TO   DEN-F48.
     MOVE     PARA-IN-ONL-NOUHIN-FR TO   DEN-F112.
     START    SHTDENF  KEY  >=    DEN-F46   DEN-F47
                                  DEN-F01   DEN-F112
                                  DEN-F48   DEN-F02
                                  DEN-F04   DEN-F051
                                  DEN-F07   DEN-F03
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*売上伝票ファイル読込
     PERFORM  SHTDENF-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     END-IF.
     MOVE    DEN-F112    TO   WK-DEN-F112.
     MOVE    DEN-F48     TO   WK-DEN-F48.
 INIT-010.
*
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    "ブレイクチェック"
     MOVE     DEN-F112            TO   HEN-DEN-F112.
     MOVE     DEN-F48             TO   HEN-DEN-F48.
     IF   HEN-DEN-F48    NOT =  WK-DEN-F48
     OR   HEN-DEN-F112   NOT =  WK-DEN-F112
          IF PARA-IN-SOUKO = WK-DEN-F48
             PERFORM  LNKONLF-REWRITE-SEC
          END-IF
          IF       END-FLG = "END"
              GO             TO   MAIN-EXIT
          END-IF
          MOVE DEN-F112           TO   WK-DEN-F112
          MOVE DEN-F48            TO   WK-DEN-F48
          MOVE ZERO               TO   CNT-RNK
          MOVE ZERO               TO   CNT-MIRNK
     END-IF.
*物流連携ＦＬＧチェック
     IF   DEN-F68                 =   "1"
          ADD   1     TO   CNT-RNK
     ELSE
          ADD   1     TO   CNT-MIRNK
     END-IF.
*売上伝票ファイル読込
     PERFORM  SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     IF PARA-IN-SOUKO = WK-DEN-F48
        IF CNT-RNK > 0  OR  CNT-MIRNK > 0
            PERFORM  LNKONLF-REWRITE-SEC
        END-IF
     END-IF.
*プログラム終了メッセージ表示
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
*    MOVE      NOT-CNT   TO      NON-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
*    DISPLAY   MSG-NON   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*ファイルクローズ
     CLOSE     SHTDENF   LNKONLF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　売上伝票ファイル読込
****************************************************************
 SHTDENF-READ-SEC          SECTION.
*
     READ     SHTDENF
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO SHTDENF-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT ") #"
                      UPON CONS
     END-IF.
*指定バッチ_チェック
 SHTDENF-READ-1.
     IF     ( PARA-IN-ONL-JUSIN-HI  =  DEN-F46 ) AND
            ( PARA-IN-ONL-JUSIN-JI  =  DEN-F47 ) AND
            ( PARA-IN-ONL-JUSIN-TOR =  DEN-F01 )
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   SHTDENF-READ-EXIT
     END-IF.
*納品日ＴＯチェック
 SHTDENF-READ-2.
     IF      PARA-IN-ONL-NOUHIN-TO >=  DEN-F112
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   SHTDENF-READ-EXIT
     END-IF.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*            オンライン連携状況ファイル更新
****************************************************************
 LNKONLF-REWRITE-SEC         SECTION.
     MOVE  "LNKONLF-REWRITE-SEC"  TO  S-NAME.

*レコード初期化
     MOVE     SPACE               TO   LNK-REC.
     INITIALIZE                        LNK-REC.
*    読み込み
     MOVE  PARA-IN-ONL-JUSIN-HI  TO  LNK-F01.
     MOVE  PARA-IN-ONL-JUSIN-JI  TO  LNK-F02.
     MOVE  PARA-IN-ONL-JUSIN-TOR TO  LNK-F03.
     MOVE  PARA-IN-SOUKO         TO  LNK-F04.
     MOVE  WK-DEN-F112           TO  LNK-F05.
     READ  LNKONLF
       INVALID
         DISPLAY "# NOT KEY ABEND #"  UPON CONS
         DISPLAY "# バッチ日付   = "  LNK-F01  UPON CONS
         DISPLAY "# バッチ時刻   = "  LNK-F02  UPON CONS
         DISPLAY "# バッチ取引先 = "  LNK-F03  UPON CONS
         DISPLAY "# 倉庫ＣＤ　   = "  LNK-F04  UPON CONS
         DISPLAY "# 納品日　　　 = "  LNK-F05  UPON CONS
         MOVE  "END"   TO   END-FLG
         MOVE    0     TO   CNT-RNK
         MOVE    0     TO   CNT-MIRNK
         GO            TO   LNKONLF-REWRITE-EXIT
     END-READ.
     MOVE   CNT-RNK              TO  LNK-F08.
     MOVE   CNT-MIRNK            TO  LNK-F09.
     MOVE   SYS-DATEW            TO  LNK-F10.
     MOVE   PARA-IN-BUMONCD      TO  LNK-F11.
     MOVE   PARA-IN-TANCD        TO  LNK-F12.
     MOVE   SYS-DATEW            TO  LNK-F98.
     MOVE   SYS-HHMNSS           TO  LNK-F99.
     REWRITE  LNK-REC.
*出力件数
     ADD   1   TO   WRT-CNT.
     MOVE  0   TO   CNT-RNK.
     MOVE  0   TO   CNT-MIRNK.
*
 LNKONLF-REWRITE-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
