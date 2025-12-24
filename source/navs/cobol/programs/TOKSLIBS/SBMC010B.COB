******************************************************************
*
* 　  顧客名             ： (株)サカタのタネ殿
*   　サブシステム名     ： ＨＧ基幹システム　
*   　業務名　　　       ： 流通ＢＭＳ
*   　モジュール名       ： メッセージ別振分処理（カインズ）
*   　作成日／更新日     ： 2014/05/21
*   　作成日／更新者     ：
*   　処理概要           ： 流通ＢＭＳ受信ファイル内の各種
*                           データを、それぞれのメッセージ
*                           ファイルへ振り分ける、また受信
*                           件数リスト出力処理への引渡しの
*                           ため、パラメタ（ＯＵＴ）をセッ
*                           トする。
*
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            SBMC010B.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/05/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*流通ＢＭＳ受信ファイル
     SELECT   BMSRCV    ASSIGN    TO   DA-01-S-BMSRCVW
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   RCV-ST.
*流通ＢＭＳ発注メッセージ
     SELECT   BMSHAC    ASSIGN         DA-01-VI-BMSHACL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  HAC-F011
                                       HAC-F012
                                       HAC-F013
                                       HAC-F02
                                       HAC-F308
                                       HAC-F346
                                       HAC-F302
                                       HAC-F402
                        FILE STATUS    IS   HAC-ST.
*流通ＢＭＳ受領メッセージ
     SELECT   BMSJYR    ASSIGN         DA-01-VI-BMSJYRL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYR-F011
                                       JYR-F012
                                       JYR-F013
                        FILE STATUS    IS   JYR-ST.
*流通ＢＭＳ受領訂正メッセージ
     SELECT   BMSJYT    ASSIGN         DA-01-VI-BMSJYTL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYT-F011
                                       JYT-F012
                                       JYT-F013
                        FILE STATUS    IS   JYT-ST.
*流通ＢＭＳ返品メッセージ
     SELECT   BMSHEP    ASSIGN         DA-01-VI-BMSHEPL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  HEP-F011
                                       HEP-F012
                                       HEP-F013
                        FILE STATUS    IS   HEP-ST.
*流通ＢＭＳ支払メッセージ
     SELECT   BMSSIH    ASSIGN         DA-01-VI-BMSSIHL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SIH-F011
                                       SIH-F012
                                       SIH-F013
                        FILE STATUS    IS   SIH-ST.
*
*↓2013/06/28
*条件ファイル
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01
                                       JYO-F02
                        FILE STATUS    IS   JYO-ST.
*↑2013/06/28
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*流通ＢＭＳ受信ファイル
******************************************************************
 FD  BMSRCV             BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
     COPY     BMSRCVF   OF        XFDLIB
              JOINING   RCV       PREFIX.
 01  RCV-REC2.
     03  RCV-F101                 PIC   X(01).
     03  RCV-F110                 PIC   X(145).
     03  RCV-F120                 PIC   X(07).
     03  RCV-FDATA                PIC   X(1047).
******************************************************************
*流通ＢＭＳ発注メッセージ
******************************************************************
 FD  BMSHAC
                        LABEL     RECORD   IS   STANDARD.
     COPY     BMSHACF   OF        XFDLIB
              JOINING   HAC       PREFIX.
******************************************************************
*流通ＢＭＳ受領メッセージ
******************************************************************
 FD  BMSJYR
                        LABEL     RECORD   IS   STANDARD.
     COPY     BMSJYRF   OF        XFDLIB
              JOINING   JYR       PREFIX.
******************************************************************
*流通ＢＭＳ受領訂正メッセージ
******************************************************************
 FD  BMSJYT
                        LABEL     RECORD   IS   STANDARD.
     COPY     BMSJYTF   OF        XFDLIB
              JOINING   JYT       PREFIX.
******************************************************************
*流通ＢＭＳ返品メッセージ
******************************************************************
 FD  BMSHEP
                        LABEL     RECORD   IS   STANDARD.
     COPY     BMSHEPF   OF        XFDLIB
              JOINING   HEP       PREFIX.
******************************************************************
*流通ＢＭＳ支払メッセージ
******************************************************************
 FD  BMSSIH
                        LABEL     RECORD   IS   STANDARD.
     COPY     BMSSIHF   OF        XFDLIB
              JOINING   SIH       PREFIX.
*↓2013/06/28
******************************************************************
*条件ファイル
******************************************************************
 FD  HJYOKEN
                        LABEL     RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*↑2013/06/28
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
*
     COPY     BMSHACF   OF        XFDLIB
              JOINING   WHAC      PREFIX.
     COPY     BMSJYRF   OF        XFDLIB
              JOINING   WJYR      PREFIX.
     COPY     BMSJYTF   OF        XFDLIB
              JOINING   WJYT      PREFIX.
     COPY     BMSHEPF   OF        XFDLIB
              JOINING   WHEP      PREFIX.
     COPY     BMSSIHF   OF        XFDLIB
              JOINING   WSIH      PREFIX.
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  RCV-ST                   PIC  X(02).
     03  HAC-ST                   PIC  X(02).
     03  JYR-ST                   PIC  X(02).
     03  JYT-ST                   PIC  X(02).
     03  HEP-ST                   PIC  X(02).
     03  SIH-ST                   PIC  X(02).
*↓2013/06/28
     03  JYO-ST                   PIC  X(02).
*↑2013/06/28
*
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
         NC"流通ＢＭＳ受信ファイルエラ－".
     03  HAC-ERR                  PIC  N(20)     VALUE
         NC"流通ＢＭＳ発注メッセージエラ－".
     03  JYR-ERR                  PIC  N(20)     VALUE
         NC"流通ＢＭＳ受領メッセージエラ－".
     03  JYT-ERR                  PIC  N(20)     VALUE
         NC"流通ＢＭＳ受領訂正メッセージエラ－".
     03  HEP-ERR                  PIC  N(20)     VALUE
         NC"流通ＢＭＳ返品メッセージエラ－".
     03  SIH-ERR                  PIC  N(20)     VALUE
         NC"流通ＢＭＳ支払メッセージエラ－".
*↓2013/06/28
     03  JYO-ERR                  PIC  N(20)     VALUE
         NC"条件ファイルエラー　　　　　　".
*↑2013/06/28
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SBMC010B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SBMC010B".
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
* 入力パラメタ
*    バッチ（受信日）
 01  PARA-IN-JUSIN-HI             PIC  9(08).
*    バッチ（受信時刻）
 01  PARA-IN-JUSIN-JI             PIC  9(04).
*    バッチ（取引先）
 01  PARA-IN-JUSIN-TOR            PIC  9(08).
* 出力パラメタ
*    受信件数（全件）　
 01  PARA-OUT-ZENKEN              PIC  9(08).
*    受信件数（発注）　
 01  PARA-OUT-HACHUU              PIC  9(08).
*    受信件数（受領）　
 01  PARA-OUT-JYURYO              PIC  9(08).
*    受信件数（受領訂正）　
 01  PARA-OUT-JYUTEI              PIC  9(08).
*    受信件数（返品）　
 01  PARA-OUT-HENPIN              PIC  9(08).
*    受信件数（支払）　
 01  PARA-OUT-SIHARA              PIC  9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-IN-JUSIN-HI
                            PARA-IN-JUSIN-JI
                            PARA-IN-JUSIN-TOR
                            PARA-OUT-ZENKEN
                            PARA-OUT-HACHUU
                            PARA-OUT-JYURYO
                            PARA-OUT-JYUTEI
                            PARA-OUT-HENPIN
                            PARA-OUT-SIHARA.
 DECLARATIVES.
 RCV-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  BMSRCV.
     DISPLAY       RCV-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       RCV-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 HAC-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  BMSHAC.
     DISPLAY       HAC-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       HAC-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 JYR-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  BMSJYR.
     DISPLAY       JYR-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       JYR-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 JYT-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  BMSJYT.
     DISPLAY       JYT-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       JYT-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 HEP-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  BMSHEP.
     DISPLAY       HEP-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       HEP-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 SIH-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  BMSSIH.
     DISPLAY       SIH-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       SIH-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
*↓013/06/28
 JYO-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  HJYOKEN.
     DISPLAY       JYO-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       JYO-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
*↑013/06/28
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
     OPEN     INPUT     BMSRCV.
     OPEN     I-O       BMSHAC.
     OPEN     I-O       BMSJYR.
     OPEN     I-O       BMSJYT.
     OPEN     I-O       BMSHEP.
     OPEN     I-O       BMSSIH.
*↓2013/06/28
     OPEN     INPUT     HJYOKEN.
*↑2013/06/28
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
     MOVE     ZERO      TO        RD-CNT    WRT-CNT1
                                            WRT-CNT2
                                            WRT-CNT3
                                            WRT-CNT4
                                            WRT-CNT5.
*
*流通ＢＭＳ受信ファイル読込
     PERFORM  BMSRCVF-READ-SEC.
*
*↓2013/06/28
*条件ファイル読込
     PERFORM  HJYOKEN-READ-SEC.
*↑2013/06/28
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理                                      *
******************************************************************
 MAIN-SEC     SECTION.
*
*    DISPLAY "MAIN-SEC"  UPON  CONS.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*メッセージ区分より振分
     IF       RCV-F101  =  "A"
              PERFORM   MSG-SKB-SEC
     END-IF.
     EVALUATE WK-MSG-DATA
         WHEN "ORDER"
           EVALUATE  RCV-F101
             WHEN  "A"
              MOVE      RCV-REC   TO   WHAC-F10
             WHEN  "B"
              MOVE      RCV-REC   TO   WHAC-F20
             WHEN  "C"
              MOVE      RCV-REC   TO   WHAC-F30
             WHEN  "D"
              MOVE      RCV-REC   TO   WHAC-F40
              MOVE      WHAC-REC  TO   HAC-REC
              WRITE                    HAC-REC
              ADD       1         TO   WRT-CNT1
           END-EVALUATE
         WHEN "RECEI"
           EVALUATE  RCV-F101
             WHEN  "A"
              MOVE      RCV-REC   TO   WJYR-F10
             WHEN  "B"
              MOVE      RCV-REC   TO   WJYR-F20
             WHEN  "C"
              MOVE      RCV-REC   TO   WJYR-F30
             WHEN  "D"
**************IF RCV-REC(822:2) = SPACE
*                MOVE   0         TO   RCV-REC(822:1)
*                MOVE   0         TO   RCV-REC(823:1)
**************END-IF
              MOVE      RCV-REC   TO   WJYR-F40
              MOVE      WJYR-REC  TO   JYR-REC
              WRITE                    JYR-REC
              ADD       1         TO   WRT-CNT2
           END-EVALUATE
         WHEN "CORRE"
           EVALUATE  RCV-F101
             WHEN  "A"
              MOVE      RCV-REC   TO   WJYT-F10
             WHEN  "B"
              MOVE      RCV-REC   TO   WJYT-F20
             WHEN  "C"
              MOVE      RCV-REC   TO   WJYT-F30
             WHEN  "D"
              MOVE      RCV-REC   TO   WJYT-F40
              MOVE      WJYT-REC  TO   JYT-REC
              WRITE                    JYT-REC
              ADD       1         TO   WRT-CNT3
           END-EVALUATE
         WHEN "RETUR"
           EVALUATE  RCV-F101
             WHEN  "A"
              MOVE      RCV-REC   TO   WHEP-F10
             WHEN  "B"
              MOVE      RCV-REC   TO   WHEP-F20
             WHEN  "C"
              MOVE      RCV-REC   TO   WHEP-F30
             WHEN  "D"
              MOVE      RCV-REC   TO   WHEP-F40
              MOVE      WHEP-REC  TO   HEP-REC
              WRITE                    HEP-REC
              ADD       1         TO   WRT-CNT4
           END-EVALUATE
         WHEN "PAYME"
           EVALUATE  RCV-F101
             WHEN  "A"
              MOVE      RCV-REC   TO   WSIH-F10
             WHEN  "B"
              MOVE      RCV-REC   TO   WSIH-F20
             WHEN  "C"
              MOVE      RCV-REC   TO   WSIH-F30
              MOVE      JYO-F04          TO   WSIH-F308
             WHEN  "D"
              MOVE      RCV-REC   TO   WSIH-F40
              MOVE      WSIH-REC  TO   SIH-REC
              WRITE     SIH-REC
              ADD       1         TO   WRT-CNT5
           END-EVALUATE
     END-EVALUATE.
     MOVE     RCV-F101            TO   WK-ZEN-KBN.
*
*流通ＢＭＳ受信ファイル読込
     PERFORM  BMSRCVF-READ-SEC.
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
     MOVE      WRT-CNT3           TO   PARA-OUT-JYUTEI.
     MOVE      WRT-CNT4           TO   PARA-OUT-HENPIN.
     MOVE      WRT-CNT5           TO   PARA-OUT-SIHARA.
     DISPLAY NC"受信総件数　＝"  RD-CNT UPON CONS.
     DISPLAY NC"発注件数　　＝"  WRT-CNT1 UPON CONS.
     DISPLAY NC"受領件数　　＝"  WRT-CNT2 UPON CONS.
     DISPLAY NC"受領訂正件数＝"  WRT-CNT3 UPON CONS.
     DISPLAY NC"返品件数　　＝"  WRT-CNT4 UPON CONS.
     DISPLAY NC"支払件数　　＝"  WRT-CNT5 UPON CONS.
*ファイルのクローズ
     CLOSE     BMSRCV   BMSHAC   BMSJYR
               BMSJYT   BMSHEP   BMSSIH.
*↓2013/06/28
     CLOSE     HJYOKEN.
*↑2013/06/28
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
******************************************************************
*            流通ＢＭＳ受信ファイル読込
******************************************************************
 BMSRCVF-READ-SEC           SECTION.
*
*    DISPLAY "BMSRCVF-READ-SEC" UPON CONS.
     MOVE    "BMSRCVF-READ-SEC"           TO   S-NAME.
*
     READ     BMSRCV
              AT  END       MOVE  "END"   TO   END-FLG
                            GO     TO     BMSRCVF-READ-EXIT
              NOT AT  END   ADD    1      TO   RD-CNT
     END-READ.
*
*件数表示
     IF       RD-CNT(6:3)   =  "000"  OR  "500"
              DISPLAY "# READ-CNT = " RD-CNT
              UPON CONS
     END-IF.
*レコード区分チェック
     IF       ( RD-CNT      =      1 )  AND
              ( RCV-F101    NOT =  "A" )
              PERFORM       RECORD-ERR-SEC
     END-IF.
     IF       ( RCV-F101    =  "A" )  AND
              ( WK-ZEN-KBN  =  "A"  OR  "B"  OR  "C" )
              PERFORM      RECORD-ERR-SEC
     END-IF.
     IF       ( RCV-F101    =  "B" )  AND
              ( WK-ZEN-KBN  =  "B"  OR  "C" )
              PERFORM      RECORD-ERR-SEC
     END-IF.
     IF       ( RCV-F101    =  "C" )  AND
              ( WK-ZEN-KBN  =  "A"  OR  "C" )
              PERFORM      RECORD-ERR-SEC
     END-IF.
*
 BMSRCVF-READ-EXIT.
     EXIT.
*
*↓2013/06/28
******************************************************************
*            条件ファイル読込
******************************************************************
 HJYOKEN-READ-SEC           SECTION.
*
     MOVE    "HJYOKEN-READ-SEC"           TO   S-NAME.
*
     MOVE     79                          TO   JYO-F01.
     MOVE    "2"                          TO   JYO-F02.
     READ     HJYOKEN
              INVALID
                      DISPLAY NC"条件ファイルなし！"   UPON CONS
                      DISPLAY NC"　ＫＥＹ１＝" JYO-F01 UPON CONS
                      DISPLAY NC"　ＫＥＹ２＝" JYO-F02 UPON CONS
                      MOVE  "END"   TO   END-FLG
                      MOVE  "4000"  TO   PROGRAM-STATUS
                      GO     TO     HJYOKEN-READ-EXIT
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*↑2013/06/28
*
******************************************************************
*メッセージ識別
******************************************************************
 MSG-SKB-SEC   SECTION.
***  DISPLAY "MSG-SKB-000 " RCV-F120 UPON  CONS.
     MOVE      RCV-F120(1:5)              TO   WK-MSG-DATA.
     EVALUATE  WK-MSG-DATA
*発注
      WHEN     "ORDER"
*         DISPLAY "MSG-SKB-SEC ORDER" UPON  CONS
          MOVE       SPACE                TO   WHAC-REC
          INITIALIZE                           WHAC-REC
          MOVE       PARA-IN-JUSIN-HI     TO   WHAC-F011
          MOVE       PARA-IN-JUSIN-JI     TO   WHAC-F012
          MOVE       PARA-IN-JUSIN-TOR    TO   WHAC-F013
          MOVE       " "                  TO   WHAC-F02
          MOVE       "2920"               TO   WHAC-F98
          MOVE       "99"                 TO   WHAC-F99
*受領
      WHEN     "RECEI"
*         DISPLAY "MSG-SKB-SEC RECEI" UPON  CONS
          MOVE       SPACE                TO   WJYR-REC
          INITIALIZE                           WJYR-REC
          MOVE       PARA-IN-JUSIN-HI     TO   WJYR-F011
          MOVE       PARA-IN-JUSIN-JI     TO   WJYR-F012
          MOVE       PARA-IN-JUSIN-TOR    TO   WJYR-F013
          MOVE       "2920"               TO   WJYR-F98
          MOVE       "99"                 TO   WJYR-F99
*受領訂正
      WHEN     "CORRE"
*         DISPLAY "MSG-SKB-SEC CORRE" UPON  CONS
          MOVE       SPACE                TO   WJYT-REC
          INITIALIZE                           WJYT-REC
          MOVE       PARA-IN-JUSIN-HI     TO   WJYT-F011
          MOVE       PARA-IN-JUSIN-JI     TO   WJYT-F012
          MOVE       PARA-IN-JUSIN-TOR    TO   WJYT-F013
          MOVE       "2920"               TO   WJYT-F98
          MOVE       "99"                 TO   WJYT-F99
*返品
      WHEN     "RETUR"
*         DISPLAY "MSG-SKB-SEC RETUR" UPON  CONS
          MOVE       SPACE                TO   WHEP-REC
          INITIALIZE                           WHEP-REC
          MOVE       PARA-IN-JUSIN-HI     TO   WHEP-F011
          MOVE       PARA-IN-JUSIN-JI     TO   WHEP-F012
          MOVE       PARA-IN-JUSIN-TOR    TO   WHEP-F013
          MOVE       "2920"               TO   WHEP-F98
          MOVE       "99"                 TO   WHEP-F99
*支払
      WHEN     "PAYME"
*         DISPLAY "MSG-SKB-SEC PAYME" UPON  CONS
          MOVE       SPACE                TO   WSIH-REC
          INITIALIZE                           WSIH-REC
          MOVE       PARA-IN-JUSIN-HI     TO   WSIH-F011
          MOVE       PARA-IN-JUSIN-JI     TO   WSIH-F012
          MOVE       PARA-IN-JUSIN-TOR    TO   WSIH-F013
          MOVE       "2920"               TO   WSIH-F98
          MOVE       "99"                 TO   WSIH-F99
*その他
      WHEN     OTHER
*         DISPLAY "MSG-SKB-SEC OTHER" UPON  CONS
          MOVE       "END"                TO   END-FLG
          MOVE       "4000"               TO   PROGRAM-STATUS
*         STOP       RUN
     END-EVALUATE.
 MSG-SKB-EXIT.
     EXIT.
******************************************************************
*レコード格納形式エラー
******************************************************************
 RECORD-ERR-SEC        SECTION.
*    DISPLAY "KAKUNOU-KEISIKI-ERR"        UPON CONS.
     MOVE    "END"                        TO   END-FLG.
     MOVE    "4001"                       TO   PROGRAM-STATUS.
 RECORD-ERR-EXIT.
     EXIT.
