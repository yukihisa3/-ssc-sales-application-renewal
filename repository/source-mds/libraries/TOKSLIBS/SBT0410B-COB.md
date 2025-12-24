# SBT0410B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0410B.COB`

## ソースコード

```cobol
******************************************************************
*
* 　  顧客名            ： (株)サカタのタネ殿
*   　サブシステム名    ： ＨＧ基幹システム　
*   　業務名　　　      ： ＬＩＮＫＳ連携(カインズ流通BMS)
*   　モジュール名      ： 出荷梱包ＭＳＧ取込
*   　作成日／更新日    ： 2014/08/13.
*   　作成日／更新者    ：
*   　処理概要          ： 出荷指示とのチェックのため、
*                          流通ＢＭＳ出荷梱包ＭＳＧから
*                          取引明細と欠品情報レコードを
*                          抽出する。
*                          レコード種類別に件数をカウントし、
*                          パラメタ（ＯＵＴ）にセットする。
*
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            SBT0410B.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/08/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*倉庫別出荷梱包ファイル
     SELECT   CNZLNK    ASSIGN    TO   DA-01-S-CNZLNK
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   LNK-ST.
*出荷梱包（取引明細）
     SELECT   SYKMEF    ASSIGN         DA-01-S-SYKMEF
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   MEI-ST.
*出荷梱包（欠品明細）
     SELECT   SYKKPF    ASSIGN         DA-01-S-SYKKPF
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   KEP-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*倉庫別出荷梱包ファイル
******************************************************************
 FD  CNZLNK             BLOCK     CONTAINS  3   RECORDS
                        LABEL     RECORD   IS   STANDARD.
     COPY     CNZLNKA   OF        XFDLIB
                JOINING LNKA      PREFIX.
     COPY     CNZLNKB   OF        XFDLIB
                JOINING LNKB      PREFIX.
     COPY     CNZLNKC   OF        XFDLIB
                JOINING LNKC      PREFIX.
     COPY     CNZLNKD   OF        XFDLIB
                JOINING LNKD      PREFIX.
     COPY     CNZLNKE   OF        XFDLIB
                JOINING LNKE      PREFIX.
     COPY     CNZLNKF   OF        XFDLIB
                JOINING LNKF      PREFIX.
     COPY     CNZLNKG   OF        XFDLIB
                JOINING LNKG      PREFIX.
 01  LNK-REC2.
     03  LNK-F01                  PIC   X(01).
*----03  LNK-F110                 PIC   X(145).
*----03  LNK-F120                 PIC   X(07).
     03  LNK-FDATA                PIC   X(1199).
******************************************************************
*出荷梱包（取引明細）
******************************************************************
 FD  SYKMEF             BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
     COPY     SYKMEF    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*出荷梱包（欠品明細）
******************************************************************
 FD  SYKKPF             BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
     COPY     SYKKPF    OF        XFDLIB
              JOINING   KEP       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                       PIC  9(08)     VALUE  ZERO.
 01  WRT-CNTA                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNTB                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNTC                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNTD                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNTE                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNTF                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNTG                     PIC  9(08)     VALUE  ZERO.
 01  WK-ZEN-KBN                   PIC  X(01)     VALUE  SPACE.
 01  WK-MSG-DATA                  PIC  X(05)     VALUE  SPACE.
*
     COPY     SYKMEF    OF        XFDLIB
              JOINING   WMEI      PREFIX.
*
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  LNK-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
     03  KEP-ST                   PIC  X(02).
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
     03  LNK-ERR                  PIC  N(20)     VALUE
         NC"倉庫別出荷梱包ファイルエラ－".
     03  MEI-ERR                  PIC  N(20)     VALUE
         NC"出荷梱包（取引明細）　　エラ－".
     03  KEP-ERR                  PIC  N(20)     VALUE
         NC"出荷梱包（欠品明細）　　エラ－".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SBT0410B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SBT0410B".
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
*    件数１（メッセージヘッダ）
 01  PARA-OUT-KENSU1              PIC  9(08).
*    件数１（出荷梱包リスト）
 01  PARA-OUT-KENSU2              PIC  9(08).
*    件数１（発注元別）
 01  PARA-OUT-KENSU3              PIC  9(08).
*    件数１（出荷梱包内容）
 01  PARA-OUT-KENSU4              PIC  9(08).
*    件数１（取引明細）
 01  PARA-OUT-KENSU5              PIC  9(08).
*    件数１（ＩＴＦ情報）
 01  PARA-OUT-KENSU6              PIC  9(08).
*    件数１（欠品情報）
 01  PARA-OUT-KENSU7              PIC  9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-OUT-KENSU1
                            PARA-OUT-KENSU2
                            PARA-OUT-KENSU3
                            PARA-OUT-KENSU4
                            PARA-OUT-KENSU5
                            PARA-OUT-KENSU6
                            PARA-OUT-KENSU7.
 DECLARATIVES.
 LNK-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CNZLNK.
     DISPLAY       LNK-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       LNK-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 MEI-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  SYKMEF.
     DISPLAY       MEI-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       MEI-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 KEP-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  SYKKPF.
     DISPLAY       KEP-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       KEP-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
*
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
     OPEN     INPUT     CNZLNK.
     OPEN     OUTPUT    SYKMEF.
     OPEN     OUTPUT    SYKKPF.
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
     MOVE     ZERO      TO        RD-CNT    WRT-CNTA
                                            WRT-CNTB
                                            WRT-CNTC
                                            WRT-CNTD
                                            WRT-CNTE
                                            WRT-CNTF
                                            WRT-CNTG.
*
*倉庫別出荷梱包ファイル読込
     PERFORM  CNZLNK-READ-SEC.
*
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
*    IF       LNK-F01   =  "A"
*             PERFORM   MSG-SKB-SEC
*    END-IF.
     EVALUATE LNK-F01
             WHEN  "A"
              ADD       1         TO   WRT-CNTA
             WHEN  "B"
              ADD       1         TO   WRT-CNTB
             WHEN  "C"
              MOVE      LNKC-F06  TO   WMEI-F30
              ADD       1         TO   WRT-CNTC
             WHEN  "D"
              ADD       1         TO   WRT-CNTD
             WHEN  "E"
              MOVE      LNKE-REC  TO   MEI-REC
              MOVE      WMEI-F30  TO   MEI-F30
              WRITE                    MEI-REC
              ADD       1         TO   WRT-CNTE
             WHEN  "F"
              ADD       1         TO   WRT-CNTF
             WHEN  "G"
              MOVE      LNKG-REC  TO   KEP-REC
              WRITE                    KEP-REC
              ADD       1         TO   WRT-CNTG
     END-EVALUATE.
*
     MOVE     LNK-F01             TO   WK-ZEN-KBN.
*
*倉庫別出荷梱包ファイル読込
     PERFORM  CNZLNK-READ-SEC.
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
     MOVE      WRT-CNTA           TO   PARA-OUT-KENSU1.
     MOVE      WRT-CNTB           TO   PARA-OUT-KENSU2.
     MOVE      WRT-CNTC           TO   PARA-OUT-KENSU3.
     MOVE      WRT-CNTD           TO   PARA-OUT-KENSU4.
     MOVE      WRT-CNTE           TO   PARA-OUT-KENSU5.
     MOVE      WRT-CNTF           TO   PARA-OUT-KENSU6.
     MOVE      WRT-CNTG           TO   PARA-OUT-KENSU7.
*    DISPLAY NC"受信総件数　＝"  RD-CNT UPON CONS.
*    DISPLAY NC"発注件数　　＝"  WRT-CNT1 UPON CONS.
*    DISPLAY NC"受領件数　　＝"  WRT-CNT2 UPON CONS.
*    DISPLAY NC"受領訂正件数＝"  WRT-CNT3 UPON CONS.
*    DISPLAY NC"返品件数　　＝"  WRT-CNT4 UPON CONS.
*    DISPLAY NC"支払件数　　＝"  WRT-CNT5 UPON CONS.
*ファイルのクローズ
     CLOSE     CNZLNK   SYKMEF   SYKKPF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
******************************************************************
*            倉庫別出荷梱包ファイル読込
******************************************************************
 CNZLNK-READ-SEC           SECTION.
*
*    DISPLAY "CNZLNK-READ-SEC" UPON CONS.
     MOVE    "CNZLNK-READ-SEC"           TO   S-NAME.
*
     READ     CNZLNK
              AT  END       MOVE  "END"   TO   END-FLG
                            GO     TO     CNZLNK-READ-EXIT
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
              ( LNK-F01     NOT =  "A" )
              PERFORM       RECORD-ERR-SEC
     END-IF.
     IF       ( LNK-F01     =  "A" )  AND
              ( WK-ZEN-KBN  =  "A"  OR  "B"  OR  "C"
                            OR "E"  OR  "F"  OR  "G" )
              PERFORM       RECORD-ERR-SEC
     END-IF.
     IF       ( LNK-F01     =  "B" )  AND
              ( WK-ZEN-KBN  =  "B" )
              PERFORM       RECORD-ERR-SEC
     END-IF.
     IF       ( LNK-F01     =  "C" )  AND
              ( WK-ZEN-KBN  =  "C"  OR  "D" )
              PERFORM       RECORD-ERR-SEC
     END-IF.
     IF       ( LNK-F01     =  "D" )  AND
              ( WK-ZEN-KBN  =  "D" )
              PERFORM       RECORD-ERR-SEC
     END-IF.
*    上記以外は詳細不明
*    IF       ( LNK-F101    =  "B" )  AND
*             ( WK-ZEN-KBN  =  "B"  OR  "C" )
*             PERFORM       RECORD-ERR-SEC
*    END-IF.
*    IF       ( LNK-F101    =  "C" )  AND
*             ( WK-ZEN-KBN  =  "A"  OR  "C" )
*             PERFORM       RECORD-ERR-SEC
*    END-IF.
*
 CNZLNK-READ-EXIT.
     EXIT.
*
******************************************************************
*メッセージ識別
******************************************************************
*MSG-SKB-SEC   SECTION.
***  DISPLAY "MSG-SKB-000 " LNK-F120 UPON  CONS.
*    MOVE      LNK-F120(1:5)              TO   WK-MSG-DATA.
*    EVALUATE  WK-MSG-DATA
*    EVALUATE  LNK-F01
*発注
*     WHEN     "ORDER"
*         DISPLAY "MSG-SKB-SEC ORDER" UPON  CONS
*         MOVE       SPACE                TO   WMEI-REC
*         INITIALIZE                           WMEI-REC
*         MOVE       PARA-IN-JUSIN-HI     TO   WMEI-F011
*         MOVE       PARA-IN-JUSIN-JI     TO   WMEI-F012
*         MOVE       PARA-IN-JUSIN-TOR    TO   WMEI-F013
*         MOVE       " "                  TO   WMEI-F02
*         MOVE       "2920"               TO   WMEI-F98
*         MOVE       "99"                 TO   WMEI-F99
*受領
*     WHEN     "RECEI"
*         DISPLAY "MSG-SKB-SEC RECEI" UPON  CONS
*         MOVE       SPACE                TO   WKEP-REC
*         INITIALIZE                           WKEP-REC
*         MOVE       PARA-IN-JUSIN-HI     TO   WKEP-F011
*         MOVE       PARA-IN-JUSIN-JI     TO   WKEP-F012
*         MOVE       PARA-IN-JUSIN-TOR    TO   WKEP-F013
*         MOVE       "2920"               TO   WKEP-F98
*         MOVE       "99"                 TO   WKEP-F99
*受領訂正
*     WHEN     "CORRE"
*         DISPLAY "MSG-SKB-SEC CORRE" UPON  CONS
*         MOVE       SPACE                TO   WJYT-REC
*         INITIALIZE                           WJYT-REC
*         MOVE       PARA-IN-JUSIN-HI     TO   WJYT-F011
*         MOVE       PARA-IN-JUSIN-JI     TO   WJYT-F012
*         MOVE       PARA-IN-JUSIN-TOR    TO   WJYT-F013
*         MOVE       "2920"               TO   WJYT-F98
*         MOVE       "99"                 TO   WJYT-F99
*返品
*     WHEN     "RETUR"
*         DISPLAY "MSG-SKB-SEC RETUR" UPON  CONS
*         MOVE       SPACE                TO   WHEP-REC
*         INITIALIZE                           WHEP-REC
*         MOVE       PARA-IN-JUSIN-HI     TO   WHEP-F011
*         MOVE       PARA-IN-JUSIN-JI     TO   WHEP-F012
*         MOVE       PARA-IN-JUSIN-TOR    TO   WHEP-F013
*         MOVE       "2920"               TO   WHEP-F98
*         MOVE       "99"                 TO   WHEP-F99
*支払
*     WHEN     "PAYME"
*         DISPLAY "MSG-SKB-SEC PAYME" UPON  CONS
*         MOVE       SPACE                TO   WSIH-REC
*         INITIALIZE                           WSIH-REC
*         MOVE       PARA-IN-JUSIN-HI     TO   WSIH-F011
*         MOVE       PARA-IN-JUSIN-JI     TO   WSIH-F012
*         MOVE       PARA-IN-JUSIN-TOR    TO   WSIH-F013
*         MOVE       "2920"               TO   WSIH-F98
*         MOVE       "99"                 TO   WSIH-F99
*その他
*     WHEN     OTHER
*         DISPLAY "MSG-SKB-SEC OTHER" UPON  CONS
*         MOVE       "END"                TO   END-FLG
*         MOVE       "4000"               TO   PROGRAM-STATUS
*         STOP       RUN
*    END-EVALUATE.
*MSG-SKB-EXIT.
*    EXIT.
******************************************************************
*レコード格納形式エラー
******************************************************************
 RECORD-ERR-SEC        SECTION.
*    DISPLAY "KAKUNOU-KEISIKI-ERR"        UPON CONS.
     MOVE    "END"                        TO   END-FLG.
     MOVE    "4001"                       TO   PROGRAM-STATUS.
 RECORD-ERR-EXIT.
     EXIT.

```
