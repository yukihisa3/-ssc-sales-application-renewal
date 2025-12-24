******************************************************************
* 　  顧客名             ： (株)サカタのタネ殿
*   　サブシステム名     ： ＨＧ基幹システム　
*   　業務名　　　       ： ＥＤＩＣシステム（共通）
*   　モジュール名       ： ＥＤＩＣ返品データ変換
*   　作成日／更新日     ： 2015/10/26
*   　作成日／更新者     ：
*   　処理概要           ： ＥＤＩＣ返品データを読み、
*                           ＥＤＩＣ返品ＭＳＧファイルを作
*                           成する。（制御バイトなしＶｅｒ）
*     更新日／更新者     ：
*     更新日／更新者     ：
*     修正概要           ：
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            NJH9121B.
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
*ＥＤＩＣ返品データ（受信）
     SELECT   CVCSG001  ASSIGN    TO   DA-01-S-CVCSG001
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   RCV-ST.
*ＥＤＩＣ返品ＭＳＧファイル（キー１）
     SELECT   EDHENPF   ASSIGN         DA-01-VI-EDHENPL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  HEP-F011
                                       HEP-F012
                                       HEP-F013
                                       HEP-F02
                                       HEP-F03
                                       HEP-F04
                                       HEP-F05
                        FILE STATUS    IS   HEP-ST.
*ＥＤＩＣ返品ＭＳＧファイル（キー２）
     SELECT   EDHENPL2  ASSIGN         DA-01-VI-EDHENPL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  HE2-F013
                        FILE STATUS    IS   HE2-ST.
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*ＥＤＩＣ返品データ（受信）
******************************************************************
 FD  CVCSG001           BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
 01  CVCSG001-REC.
     03  CVCSG001-F01             PIC   X(004).
     03  CVCSG001-FIL             PIC   X(380).
******************************************************************
*ＥＤＩＣ返品ＭＳＧファイル（キー１）
******************************************************************
 FD  EDHENPF
                        LABEL     RECORD   IS   STANDARD.
     COPY     EDHENPF   OF        XFDLIB
              JOINING   HEP       PREFIX.
******************************************************************
*ＥＤＩＣ返品ＭＳＧファイル（キー２）
******************************************************************
 FD  EDHENPL2
                        LABEL     RECORD   IS   STANDARD.
     COPY     EDHENPF   OF        XFDLIB
              JOINING   HE2       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                       PIC  9(08)     VALUE  ZERO.
 01  DEL-CNT                      PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                      PIC  9(08)     VALUE  ZERO.
*
     COPY     EDHENPF   OF        XFDLIB
              JOINING   WHEP      PREFIX.
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  RCV-ST                   PIC  X(02).
     03  HEP-ST                   PIC  X(02).
     03  HE2-ST                   PIC  X(02).
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
 01  WK-WHEP-F212                 PIC  X(05).
 01  FILLER                       REDEFINES      WK-WHEP-F212.
     03  HK-WHEP-F212             PIC  9(05).
****    検収日
 01  WK-WHEP-F230                 PIC  X(08).
 01  FILLER                       REDEFINES      WK-WHEP-F230.
     03  HK-WHEP-F230             PIC  9(08).
****    伝票番号
 01  WK-WHEP-F208                 PIC  X(10).
 01  FILLER                       REDEFINES      WK-WHEP-F208.
     03  HK-WHEP-F208             PIC  9(10).
****    行番号
 01  WK-WHEP-F302                 PIC  X(02).
 01  FILLER                       REDEFINES      WK-WHEP-F302.
     03  HK-WHEP-F302             PIC  9(02).
***  セクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  RCV-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ返品データ（受信）エラー".
     03  HEP-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ返品ＭＳＧ（キー１）エラー".
     03  HE2-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ返品ＭＳＧ（キー２）エラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NJH9121B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NJH9121B".
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
 01  PARA-IN-JUSIN-HI             PIC  9(08).
 01  PARA-IN-JUSIN-JI             PIC  9(04).
 01  PARA-IN-JUSIN-TOR            PIC  9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-IN-JUSIN-HI
                            PARA-IN-JUSIN-JI
                            PARA-IN-JUSIN-TOR.
 DECLARATIVES.
 RCV-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CVCSG001.
     DISPLAY       RCV-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       RCV-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 HEP-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EDHENPF.
     DISPLAY       HEP-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       HEP-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 HE2-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EDHENPL2.
     DISPLAY       HE2-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       HE2-ST   UPON      CONS.
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
*ＥＤＩＣ返品ＭＳＧ内取引先ＣＤ削除
     PERFORM EDHENPL2-DEL-SEC.
*
     OPEN     INPUT     CVCSG001.
     OPEN     I-O       EDHENPF.
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
*ＥＤＩＣ返品データ読込
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
         WHEN  "4000"
                INITIALIZE                 WHEP-REC
                MOVE  CVCSG001-REC     TO  WHEP-F100
         WHEN  "4001"
                INITIALIZE                 WHEP-F200
                MOVE  CVCSG001-REC     TO  WHEP-F200
         WHEN  "4002"
                INITIALIZE                 WHEP-F300
                MOVE  CVCSG001-REC         TO  WHEP-F300
                MOVE  CVCSG001-REC(1:62)   TO  WHEP-F300(1:62)
                MOVE  X"28"                TO  WHEP-F300(63:1)
                MOVE  CVCSG001-REC(63:24)  TO  WHEP-F300(64:24)
                MOVE  X"29"                TO  WHEP-F300(88:1)
                MOVE  SPACE                TO  WHEP-F300(89:1)
                MOVE  CVCSG001-REC(88:117) TO  WHEP-F300(90:117)
****************ファイル出力する準備を行う
****************初期化
                MOVE  SPACE            TO  HEP-REC
                INITIALIZE                 HEP-REC
****************ファイルヘッダー転送
                MOVE  WHEP-F100        TO  HEP-F100
****************返品ヘッダー転送
                MOVE  WHEP-F200        TO  HEP-F200
****************返品明細　　転送
                MOVE  WHEP-F300        TO  HEP-F300
****************バッチ番号　転送
                MOVE  PARA-IN-JUSIN-HI TO  HEP-F011
                MOVE  PARA-IN-JUSIN-JI TO  HEP-F012
                MOVE  PARA-IN-JUSIN-TOR TO HEP-F013
****************店舗コード　転送
                MOVE  WHEP-F212(9:5)   TO  WK-WHEP-F212
                MOVE  HK-WHEP-F212     TO  HEP-F02
****************検収日　　　転送
                MOVE  WHEP-F230        TO  WK-WHEP-F230
                MOVE  HK-WHEP-F230     TO  HEP-F230
****************伝票番号　　転送
                MOVE  WHEP-F208        TO  WK-WHEP-F208
                MOVE  HK-WHEP-F208     TO  HEP-F208
****************行番号　　　転送
                MOVE  WHEP-F302        TO  WK-WHEP-F302
                MOVE  HK-WHEP-F302     TO  HEP-F302
                WRITE HEP-REC
                ADD   1                TO  WRT-CNT
     END-EVALUATE.
*
*ＥＤＩＣ返品データ読込
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
     DISPLAY NC"返品ＭＳＧ作成"  " = " WRT-CNT  UPON CONS.
*ファイルのクローズ
     CLOSE     CVCSG001  EDHENPF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
******************************************************************
*ＥＤＩＣ返品データ読込
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
         DISPLAY "## NJH9121B RD-CNT = " RD-CNT  UPON CONS
     END-IF.
*
 CVCSG001-READ-EXIT.
     EXIT.
******************************************************************
*ＥＤＩＣ返品ＭＳＧファイル削除処理（スタート）
******************************************************************
 EDHENPL2-DEL-SEC       SECTION.
     MOVE    "EDHENPF-DEL-SEC"            TO   S-NAME.
*ファイルをＯＰＥＮする。
     OPEN  I-O  EDHENPL2.
*スタートをかける
     MOVE     SPACE                       TO   HE2-REC.
     INITIALIZE                                HE2-REC.
     START  EDHENPL2  KEY  IS  >=  HE2-F013
            INVALID
            MOVE      "END"               TO   END-FLG
            GO                            TO   EDHENPL2-DEL-010
     END-START.
*ファイルを読込む
     PERFORM  EDHENPL2-READ-SEC  UNTIL  END-FLG = "END".
 EDHENPL2-DEL-010.
*ファイルをＣＬＯＳＥする。
     CLOSE  EDHENPL2.
*
 EDHENPL2-DEL-EXIT.
     EXIT.
******************************************************************
*ＥＤＩＣ返品ＭＳＧファイル削除処理（削除）
******************************************************************
 EDHENPL2-READ-SEC     SECTION.
     MOVE    "EDHENPL2-READ-SEC"          TO   S-NAME.
*スタートをかける
     READ  EDHENPL2  NEXT  AT  END
                     MOVE  "END"          TO   END-FLG
                     GO                   TO   EDHENPL2-READ-EXIT
     END-READ.
*取引先ＣＤをチェックする
     IF  PARA-IN-JUSIN-TOR  NOT =  HE2-F013
         MOVE  "END"                      TO   END-FLG
         GO                               TO   EDHENPL2-READ-EXIT
     END-IF.
*ファイルを削除する
     DELETE  EDHENPL2.
     ADD     1                            TO   DEL-CNT.
*
 EDHENPL2-READ-EXIT.
     EXIT.
