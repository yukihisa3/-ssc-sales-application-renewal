******************************************************************
* 　  顧客名             ： (株)サカタのタネ殿
*   　サブシステム名     ： ＨＧ基幹システム　
*   　業務名　　　       ： ＥＤＩＣシステム（共通）
*   　モジュール名       ： ＥＤＩＣ支払データ変換
*   　作成日／更新日     ： 2015/08/20
*   　作成日／更新者     ：
*   　処理概要           ： ＥＤＩＣ支払データを読み、
*                           ＥＤＩＣ支払ＭＳＧファイルを作
*                           成する。（制御バイトなしＶｅｒ）
*     更新日／更新者     ：
*     更新日／更新者     ：
*     修正概要           ：
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            NJH9131B.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/08/19.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＥＤＩＣ支払データ（受信）
     SELECT   CVCSG001  ASSIGN    TO   DA-01-S-CVCSG001
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   RCV-ST.
*ＥＤＩＣ支払ＭＳＧファイル（キー１）
     SELECT   EDSIHAF   ASSIGN         DA-01-VI-EDSIHAL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SIH-F011
                                       SIH-F012
                                       SIH-F013
                                       SIH-F02
                        FILE STATUS    IS   SIH-ST.
*ＥＤＩＣ支払ＭＳＧファイル（キー２）
     SELECT   EDSIHAL2  ASSIGN         DA-01-VI-EDSIHAL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SI2-F013
                        FILE STATUS    IS   SI2-ST.
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*ＥＤＩＣ支払データ（受信）
******************************************************************
 FD  CVCSG001           BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
 01  CVCSG001-REC.
     03  CVCSG001-F01             PIC   X(004).
     03  CVCSG001-FIL             PIC   X(380).
******************************************************************
*ＥＤＩＣ支払ＭＳＧファイル（キー１）
******************************************************************
 FD  EDSIHAF
                        LABEL     RECORD   IS   STANDARD.
     COPY     EDSIHAF   OF        XFDLIB
              JOINING   SIH       PREFIX.
******************************************************************
*ＥＤＩＣ支払ＭＳＧファイル（キー２）
******************************************************************
 FD  EDSIHAL2
                        LABEL     RECORD   IS   STANDARD.
     COPY     EDSIHAF   OF        XFDLIB
              JOINING   SI2       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                       PIC  9(08)     VALUE  ZERO.
 01  DEL-CNT                      PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                      PIC  9(08)     VALUE  ZERO.
*
     COPY     EDSIHAF   OF        XFDLIB
              JOINING   WSIH      PREFIX.
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  RCV-ST                   PIC  X(02).
     03  SIH-ST                   PIC  X(02).
     03  SI2-ST                   PIC  X(02).
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
****  文字変換
****    店舗ＣＤ
 01  WK-WSIH-F212                 PIC  X(05).
 01  FILLER                       REDEFINES      WK-WSIH-F212.
     03  HK-WSIH-F212             PIC  9(05).
****    検収日
 01  WK-WSIH-F210                 PIC  X(08).
 01  FILLER                       REDEFINES      WK-WSIH-F210.
     03  HK-WSIH-F210             PIC  9(08).
****    伝票番号
 01  WK-WSIH-F208                 PIC  X(10).
 01  FILLER                       REDEFINES      WK-WSIH-F208.
     03  HK-WSIH-F208             PIC  9(10).
****    行番号
 01  WK-WSIH-F302                 PIC  X(02).
 01  FILLER                       REDEFINES      WK-WSIH-F302.
     03  HK-WSIH-F302             PIC  9(02).
***  セクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  RCV-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ支払データ（受信）エラー".
     03  SIH-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ支払ＭＳＧ（キー１）エラー".
     03  SI2-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ支払ＭＳＧ（キー２）エラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NJH9131B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NJH9131B".
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
 01  PARA-DATA.
     03  PARA-IN-JUSIN-HI   PIC  9(08).
     03  PARA-IN-JUSIN-JI   PIC  9(04).
     03  PARA-IN-JUSIN-TOR  PIC  9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-DATA.
 DECLARATIVES.
 RCV-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CVCSG001.
     DISPLAY       RCV-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       RCV-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 SIH-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EDSIHAF.
     DISPLAY       SIH-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       SIH-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 SI2-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EDSIHAL2.
     DISPLAY       SI2-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       SIH-ST   UPON      CONS.
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
*
     MOVE     "INIT-SEC"           TO   S-NAME.
*ＥＤＩＣ支払ＭＳＧ内取引先ＣＤ削除
     PERFORM EDSIHAL2-DEL-SEC.
*
     OPEN     INPUT     CVCSG001.
     OPEN     I-O       EDSIHAF.
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
*ＥＤＩＣ支払データ読込
     PERFORM  CVCSG001-READ-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理                                      *
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*レコード種別にてり振分
     EVALUATE  CVCSG001-F01
         WHEN  "6000"
                INITIALIZE                 WSIH-REC
                MOVE  CVCSG001-REC     TO  WSIH-F100
         WHEN  "6001"
                INITIALIZE                 WSIH-F200
                MOVE  CVCSG001-REC     TO  WSIH-F200
****************ファイル出力する準備を行う
****************初期化
                MOVE  SPACE            TO  SIH-REC
                INITIALIZE                 SIH-REC
****************ファイルヘッダー転送
                MOVE  WSIH-F100        TO  SIH-F100
****************支払ヘッダー転送
****************MOVE  WSIH-F200        TO  SIH-F200
                MOVE  WSIH-F200(1:265) TO  SIH-F200(1:265)
                MOVE  X"28"            TO  SIH-F200(266:1)
                MOVE  WSIH-F200(266:24) TO SIH-F200(267:24)
                MOVE  X"29"            TO  SIH-F200(291:1)
                MOVE  WSIH-F200(290:93) TO SIH-F200(292:93)
****************バッチ番号　転送
                MOVE  PARA-IN-JUSIN-HI TO  SIH-F011
                MOVE  PARA-IN-JUSIN-JI TO  SIH-F012
                MOVE  PARA-IN-JUSIN-TOR TO SIH-F013
****************締日　　　　転送
                MOVE  WSIH-F210        TO  WK-WSIH-F210
                MOVE  HK-WSIH-F210     TO  SIH-F02
                WRITE SIH-REC
                ADD   1                TO  WRT-CNT
     END-EVALUATE.
*
*ＥＤＩＣ支払データ読込
     PERFORM  CVCSG001-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*              終了処理                                        *
******************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*件数表示
     DISPLAY NC"初回削除　件数"  " = " DEL-CNT  UPON CONS.
     DISPLAY NC"受信Ｆ　　件数"  " = " RD-CNT   UPON CONS.
     DISPLAY NC"支払ＭＳＧ作成"  " = " WRT-CNT  UPON CONS.
*ファイルのクローズ
     CLOSE     CVCSG001  EDSIHAF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
******************************************************************
*ＥＤＩＣ支払データ読込
******************************************************************
 CVCSG001-READ-SEC      SECTION.
     MOVE    "CVCSG001-READ-SEC"          TO   S-NAME.
*ファイル読込
     READ  CVCSG001
           AT       END MOVE  "END"       TO   END-FLG
                        GO                TO   CVCSG001-READ-EXIT
     END-READ.
*
     ADD            1                     TO   RD-CNT.
     IF  RD-CNT(6:3) = "000" OR "500"
         DISPLAY "## NJH9131B RD-CNT = " RD-CNT  UPON CONS
     END-IF.
*
 CVCSG001-READ-EXIT.
     EXIT.
******************************************************************
*ＥＤＩＣ支払ＭＳＧファイル削除処理（スタート）
******************************************************************
 EDSIHAL2-DEL-SEC       SECTION.
     MOVE    "EDSIHAF-DEL-SEC"            TO   S-NAME.
*ファイルをＯＰＥＮする。
     OPEN  I-O  EDSIHAL2.
*スタートをかける
     MOVE     SPACE                       TO   SI2-REC.
     INITIALIZE                                SI2-REC.
     MOVE     PARA-IN-JUSIN-TOR           TO   SI2-F013.
     START  EDSIHAL2  KEY  IS  >=  SI2-F013
            INVALID
            MOVE      "END"               TO   END-FLG
            GO                            TO   EDSIHAL2-DEL-010
     END-START.
*ファイルを読込む
     PERFORM  EDSIHAL2-READ-SEC  UNTIL  END-FLG = "END".
 EDSIHAL2-DEL-010.
*ファイルをＣＬＯＳＥする。
     CLOSE  EDSIHAL2.
*
 EDSIHAL2-DEL-EXIT.
     EXIT.
******************************************************************
*ＥＤＩＣ支払ＭＳＧファイル削除処理（削除）
******************************************************************
 EDSIHAL2-READ-SEC     SECTION.
     MOVE    "EDSIHAL2-READ-SEC"          TO   S-NAME.
*スタートをかける
     READ  EDSIHAL2  NEXT  AT  END
                     MOVE  "END"          TO   END-FLG
                     GO                   TO   EDSIHAL2-READ-EXIT
     END-READ.
*取引先ＣＤをチェックする
     IF  PARA-IN-JUSIN-TOR  NOT =  SI2-F013
         MOVE  "END"                      TO   END-FLG
         GO                               TO   EDSIHAL2-READ-EXIT
     END-IF.
*ファイルを削除する
     DELETE  EDSIHAL2.
     ADD     1                            TO   DEL-CNT.
*
 EDSIHAL2-READ-EXIT.
     EXIT.
