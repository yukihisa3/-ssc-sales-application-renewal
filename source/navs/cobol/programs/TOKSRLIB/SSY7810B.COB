******************************************************************
* 　  顧客名             ： (株)サカタのタネ殿
*   　サブシステム名     ： ＮＡＶＳ基幹システム　
*   　業務名　　　       ： 流通ＢＭＳ（ＰＶＴ）ロイヤルＨＣ
*   　モジュール名       ： メッセージ別振分処理
*   　作成日／更新日     ： 2023/03/02
*   　作成者／更新者     ： 高橋
*   　処理概要           ： ロイヤルＨＣの受信データをデー
*                           タ種別毎に振分を実施し、件数を
*                           後続処理への連携を行なう。
*                           ※日本語制御バイト排除版
*     更新日／更新者     ： 2024/03/04 NAV TAKAHASHI
*     修正概要           ： 支払ＤＴ発生時にオープンへ変更
*
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            SSY7810B.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          13/01/18.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*流通ＢＭＳ受信ファイル（ロイヤルＨＣ）
     SELECT   ONLNWROY  ASSIGN    TO   DA-01-S-ONLNWROY
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   RCV-ST.
*ロイヤルＨＣ　発注データ
     SELECT   ONLROYAL  ASSIGN         DA-01-S-ONLROYAL
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   HAC-ST.
*ロイヤルＨＣ　受領データ
     SELECT   ROYALJYR  ASSIGN         DA-01-S-ROYALJYR
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   JYR-ST.
*ロイヤルＨＣ　支払データ
     SELECT   ROYALSIH  ASSIGN         DA-01-S-ROYALSIH
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   SIH-ST.
*ロイヤルＨＣ　欠品修正データ
     SELECT   ROYALKEP  ASSIGN         DA-01-S-ROYALKEP
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   KEP-ST.
*ロイヤルＨＣ　対象外
     SELECT   ROYALHOK  ASSIGN         DA-01-S-ROYALHOK
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   HOK-ST.
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*流通ＢＭＳ受信ファイル（ロイヤルＨＣ）
******************************************************************
 FD  ONLNWROY             BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
 01  RCV-REC.
     03  RCV-F01                  PIC   X(02).
     03  RCV-F02                  PIC   X(02).
     03  RCV-F03                  PIC   X(252).
******************************************************************
*ロイヤルＨＣ　発注データ
******************************************************************
 FD  ONLROYAL             BLOCK     CONTAINS  1   RECORDS
                          LABEL     RECORD   IS   STANDARD.
 01  HAC-REC.
     03  FILLER                   PIC   X(256).
******************************************************************
*ロイヤルＨＣ　受領データ
******************************************************************
 FD  ROYALJYR             BLOCK     CONTAINS  1   RECORDS
                          LABEL     RECORD   IS   STANDARD.
 01  JYR-REC.
     03  FILLER                   PIC   X(256).
******************************************************************
*ロイヤルＨＣ　支払データ
******************************************************************
 FD  ROYALSIH             BLOCK     CONTAINS  1   RECORDS
                          LABEL     RECORD   IS   STANDARD.
 01  SIH-REC.
     03  FILLER                   PIC   X(256).
******************************************************************
*ロイヤルＨＣ　欠品修正データ
******************************************************************
 FD  ROYALKEP             BLOCK     CONTAINS  1   RECORDS
                          LABEL     RECORD   IS   STANDARD.
 01  KEP-REC.
     03  FILLER                   PIC   X(256).
******************************************************************
*ロイヤルＨＣ　対象外
******************************************************************
 FD  ROYALHOK             BLOCK     CONTAINS  1   RECORDS
                          LABEL     RECORD   IS   STANDARD.
 01  HOK-REC.
     03  FILLER                   PIC   X(256).
*
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                       PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT3                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT4                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT5                     PIC  9(08)     VALUE  ZERO.
 01  WK-ZEN-KBN                   PIC  X(01)     VALUE  SPACE.
 01  WK-MSG-DATA                  PIC  X(05)     VALUE  SPACE.
 01  DATA-KBN                     PIC  X(02)     VALUE  SPACE.
*#2024/03/04 NAV ST
 01  OPEN-FLG                     PIC  X(01)     VALUE  SPACE.
*#2024/03/04 NAV ED
*
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  RCV-ST                   PIC  X(02).
     03  HAC-ST                   PIC  X(02).
     03  JYR-ST                   PIC  X(02).
     03  SIH-ST                   PIC  X(02).
     03  KEP-ST                   PIC  X(02).
     03  HOK-ST                   PIC  X(02).
*バッチ
*****  システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC  9(06)     VALUE  ZERO.
     03  SYS-DATEW                PIC  9(08)     VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC  9(04).
         05  SYS-MM               PIC  9(02).
         05  SYS-DD               PIC  9(02).
*****  システム時刻ワーク
 01  SYS-TIME                     PIC  9(08).
 01  FILLER                       REDEFINES      SYS-TIME.
     03  SYS-HHMMSS               PIC  9(06).
     03  SYS-MS                   PIC  9(02).
***  セクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  RCV-ERR                  PIC  N(20)     VALUE
         NC"ロイヤル　受信ファイルエラ－".
     03  HAC-ERR                  PIC  N(20)     VALUE
         NC"ロイヤル　発注ファイルエラ－".
     03  JYR-ERR                  PIC  N(20)     VALUE
         NC"ロイヤル　受領ファイルエラ－".
     03  SIH-ERR                  PIC  N(20)     VALUE
         NC"ロイヤル　支払ファイルエラ－".
     03  KEP-ERR                  PIC  N(20)     VALUE
         NC"ロイヤル　欠品修正ファイルエラ－".
     03  HOK-ERR                  PIC  N(20)     VALUE
         NC"ロイヤル　対象ファイルエラ－".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SSY7810B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SSY7810B".
         05  FILLER               PIC  X(11)     VALUE
                                        " END   *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN              PIC  X(01).
     03  LINK-IN-YMD6             PIC  9(06).
     03  LINK-IN-YMD8             PIC  9(08).
     03  LINK-OUT-RET             PIC  X(01).
     03  LINK-OUT-YMD8            PIC  9(08).
*パラメタ定義
 LINKAGE                SECTION.
* 出力パラメタ
*    受信件数（全件）　
 01  PARA-OUT-ZENKEN              PIC  9(08).
*    受信件数（発注）　
 01  PARA-OUT-HACHUU              PIC  9(08).
*    受信件数（受領）　
 01  PARA-OUT-JYURYO              PIC  9(08).
*    受信件数（支払）　　　
 01  PARA-OUT-SHIHARAI            PIC  9(08).
*    受信件数（欠品修正）
 01  PARA-OUT-KEPPIN              PIC  9(08).
*    受信件数（対象外）　　
 01  PARA-OUT-HOKA                PIC  9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
*PROCEDURE DIVISION.
 PROCEDURE DIVISION USING   PARA-OUT-ZENKEN
                            PARA-OUT-HACHUU
                            PARA-OUT-JYURYO
                            PARA-OUT-SHIHARAI
                            PARA-OUT-KEPPIN
                            PARA-OUT-HOKA.
 DECLARATIVES.
 RCV-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  ONLNWROY.
     DISPLAY       RCV-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       RCV-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 HAC-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  ONLROYAL.
     DISPLAY       HAC-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       HAC-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 JYR-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  ROYALJYR.
     DISPLAY       JYR-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       JYR-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 SIH-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  ROYALSIH.
     DISPLAY       SIH-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       SIH-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 KEP-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  ROYALKEP.
     DISPLAY       KEP-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       KEP-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 HOK-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  ROYALHOK.
     DISPLAY       HOK-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       HOK-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 END       DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
*    DISPLAY "GENERAL-PROCESS" UPON CONS.
     MOVE     "PROCESS-START"      TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =    "END".
     PERFORM  END-SEC.
*
******************************************************************
*             初期処理                                         *
******************************************************************
 INIT-SEC               SECTION.
*    DISPLAY  "INIT-SEC"  UPON  CONS.
     MOVE     "INIT-SEC"           TO   S-NAME.
     OPEN     INPUT     ONLNWROY.
     OPEN     OUTPUT    ONLROYAL.
     OPEN     OUTPUT    ROYALJYR.
*#2024/03/04 NAV ST ここではＯＰＥＮしない
*****OPEN     OUTPUT    ROYALSIH.
*#2024/03/04 NAV ED
     OPEN     OUTPUT    ROYALKEP.
     OPEN     OUTPUT    ROYALHOK.
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
              MOVE      ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE     TO        END-FLG.
*#2024/03/04 NAV ST
     MOVE     SPACE     TO        OPEN-FLG.
*#2024/03/04 NAV ED
     MOVE     ZERO      TO        RD-CNT    WRT-CNT1
                                            WRT-CNT2
                                            WRT-CNT3
                                            WRT-CNT4.
*
*流通ＢＭＳ受信ファイル読込
     PERFORM  ONLNWROY-READ-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理                                      *
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*データ種別により出力先振分
     EVALUATE DATA-KBN
         WHEN "01"     *>発注データ
               MOVE     SPACE     TO   HAC-REC
               MOVE     RCV-REC   TO   HAC-REC
               WRITE    HAC-REC
               ADD      1         TO   WRT-CNT1
         WHEN "03"     *>受領データ
               MOVE     SPACE     TO   JYR-REC
               MOVE     RCV-REC   TO   JYR-REC
               WRITE    JYR-REC
               ADD      1         TO   WRT-CNT2
         WHEN "05"     *>支払データ
***************#2024/03/04 NAV ST 支払データが発生したらＯＰＥＮ
               IF  OPEN-FLG  =  SPACE
                   OPEN     OUTPUT    ROYALSIH
                   MOVE "1"       TO   OPEN-FLG
               END-IF
***************#2024/03/04 NAV ED
               MOVE     SPACE     TO   SIH-REC
               MOVE     RCV-REC   TO   SIH-REC
               WRITE    SIH-REC
               ADD      1         TO   WRT-CNT3
         WHEN "04"     *>欠品修正データ
               MOVE     SPACE     TO   KEP-REC
               MOVE     RCV-REC   TO   KEP-REC
               WRITE    KEP-REC
               ADD      1         TO   WRT-CNT4
         WHEN OTHER    *>対象外
               MOVE     SPACE     TO   HOK-REC
               MOVE     RCV-REC   TO   HOK-REC
               WRITE    HOK-REC
               ADD      1         TO   WRT-CNT5
     END-EVALUATE.
*
*流通ＢＭＳ受信ファイル読込
     PERFORM  ONLNWROY-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*              終了処理                                        *
******************************************************************
 END-SEC       SECTION.
*
*    DISPLAY "END-SEC" UPON  CONS.
     MOVE     "END-SEC"           TO   S-NAME.
*パラメタに各件数をセット
     MOVE      RD-CNT             TO   PARA-OUT-ZENKEN.
     MOVE      WRT-CNT1           TO   PARA-OUT-HACHUU.
     MOVE      WRT-CNT2           TO   PARA-OUT-JYURYO.
     MOVE      WRT-CNT3           TO   PARA-OUT-SHIHARAI.
     MOVE      WRT-CNT4           TO   PARA-OUT-KEPPIN.
     MOVE      WRT-CNT5           TO   PARA-OUT-HOKA.
     DISPLAY NC"受信総件数　＝"  RD-CNT UPON CONS.
     DISPLAY NC"発注件数　　＝"  WRT-CNT1 UPON CONS.
     DISPLAY NC"受領件数　　＝"  WRT-CNT2 UPON CONS.
     DISPLAY NC"支払件数　　＝"  WRT-CNT3 UPON CONS.
     DISPLAY NC"欠品修正件数＝"  WRT-CNT4 UPON CONS.
     DISPLAY NC"対象外件数　＝"  WRT-CNT5 UPON CONS.
*ファイルのクローズ
*#2024/03/04 NAV ST 支払ＦのOＰＥＮ方法を変更
*****CLOSE     ONLNWROY   ONLROYAL  ROYALJYR  ROYALSIH
*****          ROYALKEP   ROYALHOK.
     CLOSE     ONLNWROY   ONLROYAL  ROYALJYR
               ROYALKEP   ROYALHOK.
     IF   OPEN-FLG  =  "1"
          CLOSE  ROYALSIH
     END-IF.
*#2024/03/04 NAV ED
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
******************************************************************
*            流通ＢＭＳ受信ファイル読込
******************************************************************
 ONLNWROY-READ-SEC           SECTION.
*
     MOVE    "ONLNWROY-READ-SEC"           TO   S-NAME.
*
     READ     ONLNWROY
              AT  END       MOVE  "END"   TO   END-FLG
                            GO     TO     ONLNWROY-READ-EXIT
**************NOT AT  END   ADD    1      TO   RD-CNT
     END-READ.
*空白レコードは読み飛ばす
*****DISPLAY "RCV-F01 = " RCV-F01 UPON CONS.
     IF       RCV-F01  =  "FL"  OR  "DH"  OR  "DD"
              CONTINUE
     ELSE
              GO            TO                 ONLNWROY-READ-SEC
     END-IF.
*
     ADD      1             TO                 RD-CNT
*件数表示
     IF       RD-CNT(6:3)   =  "000"  OR  "500"
              DISPLAY "# READ-CNT = " RD-CNT
              UPON CONS
     END-IF.
*レコード区分チェック　ＦＬ場合、データ種別セット
     IF       RCV-F01  =  "FL"
              MOVE     RCV-F02     TO     DATA-KBN
     END-IF.
*
 ONLNWROY-READ-EXIT.
     EXIT.
