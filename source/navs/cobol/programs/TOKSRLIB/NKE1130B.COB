******************************************************************
*
* 　  顧客名             ： (株)サカタのタネ殿
*   　業務名　　　       ： ＨＧ基幹システム
*   　サブシステム名     ： 流通ＢＭＳ（カインズTC1)
*   　モジュール名       ： 出荷梱包送信データ作成
*   　作成日／作成者     ： 2019/07/12 NAV
*   　処理概要           ： センター納品用各種データより、
*                           BMS標準フォーマットに編集作成する。
*                           (出荷梱包ＭＳＧ）
*     　　　　           　
*     更新日／更新者     ：
*     修正概要           ：
*     　　　　           ：
*
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            NKE1130B.
*     流用元　　        SBM0082B
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/07/12.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*検品紐付情報（カインズ）
     SELECT   CZKONXX2  ASSIGN         DA-01-VI-CZKONXX2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  KON-F01
                                       KON-F02
                                       KON-F03
                                       KON-F04
                                       KON-F07
                                       KON-F05
                                       KON-F06
                                       KON-F08
                                       KON-F09
                                       KON-F10
                                       KON-F11
                        FILE STATUS    IS   KON-ST.
*カインズ欠品データ（ＴＣ１)
     SELECT   CZKEPTC1  ASSIGN         DA-01-VI-CZKEPTC1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  KEP-F011
                                       KEP-F012
                                       KEP-F013
                                       KEP-F02
                                       KEP-F349
                                       KEP-F348
                                       KEP-F305
                                       KEP-F309
                                       KEP-F302
                                       KEP-F402
                        FILE STATUS    IS   KEP-ST.
*カインズ出荷データ
     SELECT   CZSYKTC6  ASSIGN         DA-01-VI-CZSYKTC6
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SYK-F011
                                       SYK-F012
                                       SYK-F013
                                       SYK-F02
                                       SYK-F349
                                       SYK-F348
                                       SYK-F305
                                       SYK-F309
                                       SYK-F302
                                       SYK-F402
                        FILE STATUS    IS   SYK-ST.
*流通ＢＭＳ発注メッセージ
     SELECT   BMSHACL3  ASSIGN         DA-01-VI-BMSHACL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  HAC-F011
                                       HAC-F012
                                       HAC-F013
                                       HAC-F02
                                       HAC-F346
                                       HAC-F308
                                       HAC-F302
                                       HAC-F402
                        FILE STATUS    IS   HAC-ST.
*出荷梱包送信ワーク
     SELECT   WKONCZXX  ASSIGN    TO   DA-01-S-WKONCZXX
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   SND-ST.
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*検品紐付情報（カインズ）
******************************************************************
 FD  CZKONXX2
                        LABEL     RECORD   IS   STANDARD.
     COPY     CZKONXX2  OF        XFDLIB
              JOINING   KON       PREFIX.
******************************************************************
*カインズ欠品データ（ＴＣ１)
******************************************************************
 FD  CZKEPTC1
                        LABEL     RECORD   IS   STANDARD.
     COPY     CZKEPTC1  OF        XFDLIB
              JOINING   KEP       PREFIX.
******************************************************************
*カインズ出荷データ
******************************************************************
 FD  CZSYKTC6
                        LABEL     RECORD   IS   STANDARD.
     COPY     CZSYKTC6  OF        XFDLIB
              JOINING   SYK       PREFIX.
******************************************************************
*流通ＢＭＳ発注メッセージ
******************************************************************
 FD  BMSHACL3
                        LABEL     RECORD   IS   STANDARD.
     COPY     BMSHACL3  OF        XFDLIB
              JOINING   HAC       PREFIX.
******************************************************************
*出荷梱包送信ワーク
******************************************************************
 FD  WKONCZXX
                        BLOCK     CONTAINS  3   RECORDS
                        LABEL     RECORD   IS   STANDARD.
     COPY     SYKSNDF   OF        XFDLIB
              JOINING   SND       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG1                     PIC  X(03)     VALUE  SPACE.
 01  END-FLG2                     PIC  X(03)     VALUE  SPACE.
 01  SYK-INV                      PIC  X(03)     VALUE  SPACE.
 01  HAC-INV                      PIC  X(03)     VALUE  SPACE.
 01  WK-KAISIBI                   PIC  X(08)     VALUE  SPACE.
 01  RD-CNT1                      PIC  9(08)     VALUE  ZERO.
 01  RD-CNT2                      PIC  9(08)     VALUE  ZERO.
 01  CNT-WRT-ALL                  PIC  9(08)     VALUE  ZERO.
 01  CNT-WRT-A                    PIC  9(08)     VALUE  ZERO.
 01  CNT-WRT-B                    PIC  9(08)     VALUE  ZERO.
 01  CNT-WRT-C                    PIC  9(08)     VALUE  ZERO.
 01  CNT-WRT-D                    PIC  9(08)     VALUE  ZERO.
 01  CNT-WRT-E                    PIC  9(08)     VALUE  ZERO.
 01  CNT-WRT-F                    PIC  9(08)     VALUE  ZERO.
 01  CNT-WRT-G                    PIC  9(08)     VALUE  ZERO.
 01  CTL-FLG                      PIC  X(03)     VALUE  SPACE.
*
 01  WRT-CNT2                     PIC  9(08)     VALUE  ZERO.
 01  WKEY.
     03  TORI-NO                  PIC  X(10)     VALUE  SPACE.
     03  LAST-CD                  PIC  X(13)     VALUE  SPACE.
     03  LAST-DATE                PIC  9(08)     VALUE  ZERO.
*
*判定・制御ＫＥＹ
*  紐付情報ファイル保管ＫＥＹ
 01  KEY-KON.
     03  KEY-KON-F.
         05  KEY-BDATE-KON-F01    PIC  9(08)     VALUE  ZERO.
         05  KEY-BTIME-KON-F02    PIC  9(04)     VALUE  ZERO.
         05  KEY-BTORI-KON-F03    PIC  9(08)     VALUE  ZERO.
         05  KEY-SOKCD-KON-F04    PIC  X(02)     VALUE  SPACE.
         05  KEY-NDATE-KON-F07    PIC  9(08)     VALUE  ZERO.
         05  KEY-CDATE-KON-F05    PIC  9(08)     VALUE  ZERO.
         05  KEY-CCODE-KON-F06    PIC  9(05)     VALUE  ZERO.
         05  KEY-NCODE-KON-F08    PIC  9(05)     VALUE  ZERO.
*    03  KEY-CDATE-KON-F05        PIC  9(08)     VALUE  ZERO.
*    03  KEY-CCODE-KON-F06        PIC  9(05)     VALUE  ZERO.
*    03  KEY-NCODE-KON-F08        PIC  9(05)     VALUE  ZERO.
     03  KEY-KONNO-KON-F09        PIC  X(24)     VALUE  SPACE.
     03  KEY-DENNO-KON-F10        PIC  9(09)     VALUE  ZERO.
     03  KEY-GYONO-KON-F11        PIC  9(02)     VALUE  ZERO.
 01  NEW-KON.
     03  NEW-KON-F.
         05  NEW-BDATE-KON-F01    PIC  9(08)     VALUE  ZERO.
         05  NEW-BTIME-KON-F02    PIC  9(04)     VALUE  ZERO.
         05  NEW-BTORI-KON-F03    PIC  9(08)     VALUE  ZERO.
         05  NEW-SOKCD-KON-F04    PIC  X(02)     VALUE  SPACE.
         05  NEW-NDATE-KON-F07    PIC  9(08)     VALUE  ZERO.
         05  NEW-CDATE-KON-F05    PIC  9(08)     VALUE  ZERO.
         05  NEW-CCODE-KON-F06    PIC  9(05)     VALUE  ZERO.
         05  NEW-NCODE-KON-F08    PIC  9(05)     VALUE  ZERO.
     03  NEW-KONNO-KON-F09        PIC  X(24)     VALUE  SPACE.
     03  NEW-DENNO-KON-F10        PIC  9(09)     VALUE  ZERO.
     03  NEW-GYONO-KON-F11        PIC  9(02)     VALUE  ZERO.
*  カインズ欠品データ保管ＫＥＹ
 01  KEY-KEP.
     03  KEY-KEP-F.
         05  KEY-BDATE-KEP-F011   PIC  9(08)     VALUE  ZERO.
         05  KEY-BTIME-KEP-F012   PIC  9(04)     VALUE  ZERO.
         05  KEY-BTORI-KEP-F013   PIC  9(08)     VALUE  ZERO.
         05  KEY-SOKCD-KEP-F02    PIC  X(02)     VALUE  SPACE.
         05  KEY-NDATE-KEP-F349   PIC  9(08)     VALUE  ZERO.
         05  KEY-CDATE-KEP-F348   PIC  9(08)     VALUE  ZERO.
         05  KEY-CCODE-KEP-F305   PIC  9(05)     VALUE  ZERO.
         05  KEY-NCODE-KEP-F309   PIC  9(05)     VALUE  ZERO.
*    03  KEY-CDATE-KEP-F348       PIC  9(08)     VALUE  ZERO.
*    03  KEY-CCODE-KEP-F305       PIC  9(05)     VALUE  ZERO.
*    03  KEY-NCODE-KEP-F309       PIC  9(05)     VALUE  ZERO.
     03  KEY-KONNO-KEP-FXXX       PIC  X(24)     VALUE  SPACE.
     03  KEY-DENNO-KEP-F302       PIC  9(09)     VALUE  ZERO.
     03  KEY-GYONO-KEP-F402       PIC  9(02)     VALUE  ZERO.
 01  NEW-KEP.
     03  NEW-KEP-F.
         05  NEW-BDATE-KEP-F011   PIC  9(08)     VALUE  ZERO.
         05  NEW-BTIME-KEP-F012   PIC  9(04)     VALUE  ZERO.
         05  NEW-BTORI-KEP-F013   PIC  9(08)     VALUE  ZERO.
         05  NEW-SOKCD-KEP-F02    PIC  X(02)     VALUE  SPACE.
         05  NEW-NDATE-KEP-F349   PIC  9(08)     VALUE  ZERO.
         05  NEW-CDATE-KEP-F348   PIC  9(08)     VALUE  ZERO.
         05  NEW-CCODE-KEP-F305   PIC  9(05)     VALUE  ZERO.
         05  NEW-NCODE-KEP-F309   PIC  9(05)     VALUE  ZERO.
     03  NEW-KONNO-KEP-FXXX       PIC  X(24)     VALUE  SPACE.
     03  NEW-DENNO-KEP-F302       PIC  9(09)     VALUE  ZERO.
     03  NEW-GYONO-KEP-F402       PIC  9(02)     VALUE  ZERO.
*  梱包ＭＳＧデータ作成ＫＥＹ
 01  KEY-SND.
     03  KEY-SND-F.
         05  KEY-BDATE-SND        PIC  9(08)     VALUE  ZERO.
         05  KEY-BTIME-SND        PIC  9(04)     VALUE  ZERO.
         05  KEY-BTORI-SND        PIC  9(08)     VALUE  ZERO.
         05  KEY-SOKCD-SND        PIC  X(02)     VALUE  SPACE.
         05  KEY-NDATE-SND        PIC  9(08)     VALUE  ZERO.
         05  KEY-CDATE-SND        PIC  9(08)     VALUE  ZERO.
         05  KEY-CCODE-SND        PIC  9(05)     VALUE  ZERO.
         05  KEY-NCODE-SND        PIC  9(05)     VALUE  ZERO.
*    03  KEY-CDATE-SND            PIC  9(08)     VALUE  ZERO.
*    03  KEY-CCODE-SND            PIC  9(05)     VALUE  ZERO.
*    03  KEY-NCODE-SND            PIC  9(05)     VALUE  ZERO.
     03  KEY-KONNO-SND            PIC  X(24)     VALUE  SPACE.
     03  KEY-DENNO-SND            PIC  9(09)     VALUE  ZERO.
     03  KEY-GYONO-SND            PIC  9(02)     VALUE  ZERO.
*
*出荷梱包ＭＳＧ＿（Ａ）メッセージヘッダ
     COPY     CNZKONA   OF        XFDLIB
              JOINING   SNDA      PREFIX.
*出荷梱包ＭＳＧ＿（Ｂ）出荷梱包リスト           直接納品先
     COPY     CNZKONB   OF        XFDLIB
              JOINING   SNDB      PREFIX.
*出荷梱包ＭＳＧ＿（Ｃ）発注元別出荷梱包リスト   最終納品先
     COPY     CNZKONC   OF        XFDLIB
              JOINING   SNDC      PREFIX.
*出荷梱包ＭＳＧ＿（Ｄ）出荷梱包内容             梱包
     COPY     CNZKOND   OF        XFDLIB
              JOINING   SNDD      PREFIX.
*出荷梱包ＭＳＧ＿（Ｅ）取引明細                 伝票・行
     COPY     CNZKONE   OF        XFDLIB
              JOINING   SNDE      PREFIX.
*出荷梱包ＭＳＧ＿（Ｆ）ＩＴＦ情報               不使用
     COPY     CNZKONF   OF        XFDLIB
              JOINING   SNDF      PREFIX.
*出荷梱包ＭＳＧ＿（Ｇ）欠品情報                 欠品伝票・行
     COPY     CNZKONG   OF        XFDLIB
              JOINING   SNDG      PREFIX.
*
*ワーク退避レコード
     COPY     CZSYKTC6  OF        XFDLIB
              JOINING   WSYK      PREFIX.
*ワーク明細データ退避
     COPY     SYKSNDF   OF        XFDLIB
              JOINING   WSND      PREFIX.
*
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  KON-ST                   PIC  X(02).
     03  KEP-ST                   PIC  X(02).
     03  SYK-ST                   PIC  X(02).
     03  HAC-ST                   PIC  X(02).
     03  SND-ST                   PIC  X(02).
 01  WK-REC                       PIC  X(1200)   VALUE  SPACE.
 01  WK-REC2                      PIC  X(1200)   VALUE  SPACE.
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
     03  KON-ERR                  PIC  N(20)     VALUE
         NC"検品紐付情報データエラ－".
     03  KEP-ERR                  PIC  N(20)     VALUE
         NC"カインズ欠品データエラ－".
     03  SYK-ERR                  PIC  N(20)     VALUE
         NC"カインズ出荷データエラ－".
     03  HAC-ERR                  PIC  N(20)     VALUE
         NC"流通ＢＭＳ発注メッセージエラ－".
     03  SND-ERR                  PIC  N(20)     VALUE
         NC"出荷梱包送信ワークエラ－".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NKE1130B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NKE1130B".
         05  FILLER               PIC  X(11)     VALUE
                                        " END   *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN              PIC  X(01).
     03  LINK-IN-YMD6             PIC  9(06).
     03  LINK-IN-YMD8             PIC  9(08).
     03  LINK-OUT-RET             PIC  X(01).
     03  LINK-OUT-YMD8            PIC  9(08).
*
*パラメタ定義
 LINKAGE                SECTION.
 01  PARA-IN-BDATE          PIC  9(08).
 01  PARA-IN-BTIME          PIC  9(04).
 01  PARA-IN-BTORI          PIC  9(08).
 01  PARA-IN-SOKCD          PIC  X(02).
 01  PARA-IN-NDATE          PIC  9(08).
 01  PARA-IN-MAISU          PIC  9(07).
 01  PARA-OUT-KENSU         PIC  9(07).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION         USING   PARA-IN-BDATE
                                    PARA-IN-BTIME
                                    PARA-IN-BTORI
                                    PARA-IN-SOKCD
                                    PARA-IN-NDATE
                                    PARA-IN-MAISU
                                    PARA-OUT-KENSU.
 DECLARATIVES.
 KON-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CZKONXX2.
     DISPLAY       KON-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       KON-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 KEP-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CZKEPTC1.
     DISPLAY       KEP-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       KEP-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 SYK-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CZSYKTC6.
     DISPLAY       SYK-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       SYK-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 SND-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  WKONCZXX.
     DISPLAY       SND-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       SND-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 HAC-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  BMSHACL3.
     DISPLAY       HAC-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       HAC-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 END       DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"      TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL   ( END-FLG1  =    "END" )
                  AND ( END-FLG2  =    "END" ).
     PERFORM  END-SEC.
*
******************************************************************
*             初期処理                                         *
******************************************************************
 INIT-SEC               SECTION.
     MOVE    "INIT-SEC"           TO        S-NAME.
*
     OPEN     INPUT     CZKONXX2.
     OPEN     I-O       CZKEPTC1
                        CZSYKTC6
                        BMSHACL3.
     OPEN     OUTPUT    WKONCZXX.
*
     DISPLAY  MSG-START UPON CONS.
*
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
     MOVE     SPACE                TO        END-FLG1 END-FLG2.
     MOVE     ZERO                 TO        RD-CNT1  RD-CNT2
                                             CNT-WRT-ALL
                                             CNT-WRT-A
                                             CNT-WRT-B
                                             CNT-WRT-C
                                             CNT-WRT-D
                                             CNT-WRT-E
                                             CNT-WRT-F
                                             CNT-WRT-G
                                             WRT-CNT2.
*
*紐付情報ファイルＳＴＡＲＴ・ＲＥＡＤ
     MOVE     SPACE                TO  END-FLG1.
     PERFORM  CZKONXX2-START-SEC.
*
     IF       END-FLG1    =       "   "
              PERFORM  CZKONXX2-READ-SEC
     END-IF.
*
     IF       END-FLG1    =       "END"
              DISPLAY NC"＃　出荷データなし　＃" UPON CONS
              MOVE    HIGH-VALUE   TO  KEY-KON
*             MOVE    ALL  9       TO  KEY-BDATE-KON-F01
*                                      KEY-BTIME-KON-F02
*                                      KEY-BTORI-KON-F03
*                                      KEY-NDATE-KON-F07
*                                      KEY-CDATE-KON-F05
*                                      KEY-CCODE-KON-F06
*                                      KEY-NCODE-KON-F08
*                                      KEY-DENNO-KON-F10
*                                      KEY-GYONO-KON-F11
*             MOVE    ALL "9"      TO  KEY-SOKCD-KON-F04
*                                      KEY-KONNO-KON-F09
     END-IF.
*    紐付情報ＫＥＹ保管
     IF       END-FLG1    =       "HIT"
              MOVE     KON-F01         TO       KEY-BDATE-KON-F01
                                                NEW-BDATE-KON-F01
              MOVE     KON-F02         TO       KEY-BTIME-KON-F02
                                                NEW-BTIME-KON-F02
              MOVE     KON-F03         TO       KEY-BTORI-KON-F03
                                                NEW-BTORI-KON-F03
              MOVE     KON-F04         TO       KEY-SOKCD-KON-F04
                                                NEW-SOKCD-KON-F04
              MOVE     KON-F07         TO       KEY-NDATE-KON-F07
                                                NEW-NDATE-KON-F07
              MOVE     KON-F05         TO       KEY-CDATE-KON-F05
                                                NEW-CDATE-KON-F05
              MOVE     KON-F06         TO       KEY-CCODE-KON-F06
                                                NEW-CCODE-KON-F06
              MOVE     KON-F08         TO       KEY-NCODE-KON-F08
                                                NEW-NCODE-KON-F08
              MOVE     KON-F09         TO       KEY-KONNO-KON-F09
                                                NEW-KONNO-KON-F09
              MOVE     KON-F10         TO       KEY-DENNO-KON-F10
                                                NEW-DENNO-KON-F10
              MOVE     KON-F11         TO       KEY-GYONO-KON-F11
                                                NEW-GYONO-KON-F11
     END-IF.
*
*
*欠品データＳＴＡＲＴ・ＲＥＡＤ
     MOVE     SPACE                TO  END-FLG2.
     PERFORM  CZKEPTC1-START-SEC.
*
     IF       END-FLG2    =       "   "
              PERFORM  CZKEPTC1-READ-SEC
     END-IF.
*
     IF       END-FLG2    =       "END"
              DISPLAY NC"＃　欠品データなし　＃" UPON CONS
              MOVE    HIGH-VALUE   TO  KEY-KEP
*             MOVE    ALL 9        TO  KEY-BDATE-KEP-F011
*                                      KEY-BTIME-KEP-F012
*                                      KEY-BTORI-KEP-F013
*                                      KEY-NDATE-KEP-F349
*                                      KEY-CDATE-KEP-F348
*                                      KEY-CCODE-KEP-F305
*                                      KEY-NCODE-KEP-F309
*                                      KEY-DENNO-KEP-F302
*                                      KEY-GYONO-KEP-F402
*             MOVE    ALL "9"      TO  KEY-SOKCD-KEP-F02
*                                      KEY-KONNO-KEP-FXXX
     END-IF.
*    欠品データＫＥＹ保管
     IF       END-FLG2    =       "HIT"
              MOVE     KEP-F011        TO     KEY-BDATE-KEP-F011
                                              NEW-BDATE-KEP-F011
              MOVE     KEP-F012        TO     KEY-BTIME-KEP-F012
                                              NEW-BTIME-KEP-F012
              MOVE     KEP-F013        TO     KEY-BTORI-KEP-F013
                                              NEW-BTORI-KEP-F013
              MOVE     KEP-F02         TO     KEY-SOKCD-KEP-F02
                                              NEW-SOKCD-KEP-F02
              MOVE     KEP-F349        TO     KEY-NDATE-KEP-F349
                                              NEW-NDATE-KEP-F349
              MOVE     KEP-F348        TO     KEY-CDATE-KEP-F348
                                              NEW-CDATE-KEP-F348
              MOVE     KEP-F305(1:5)   TO     KEY-CCODE-KEP-F305
                                              NEW-CCODE-KEP-F305
              MOVE     KEP-F309(1:5)   TO     KEY-NCODE-KEP-F309
                                              NEW-NCODE-KEP-F309
              MOVE     ALL "9"         TO     KEY-KONNO-KEP-FXXX
                                              NEW-KONNO-KEP-FXXX
              MOVE     KEP-F302        TO     KEY-DENNO-KEP-F302
                                              NEW-DENNO-KEP-F302
              MOVE     KEP-F402        TO     KEY-GYONO-KEP-F402
                                              NEW-GYONO-KEP-F402
     END-IF.
*
*続行判定
     IF     ( END-FLG1  =    "END" )  AND
            ( END-FLG2  =    "END" )
              DISPLAY NC"＃　対象データなし　＃" UPON CONS
              MOVE      "4010"        TO     PROGRAM-STATUS
              STOP       RUN
     END-IF.
*
*メッセージヘッダ（Ａ）出力
     IF       END-FLG2  =    "HIT"
              MOVE     "KEP"     TO    CTL-FLG
     END-IF.
     IF       END-FLG1  =    "HIT"
              MOVE     "KON"     TO    CTL-FLG
     END-IF.
     PERFORM  MSG-OUT-A-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理                                      *
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     "MAIN-SEC"           TO        S-NAME.
*
 MAIN-01.
*T↓
*    DISPLAY "KEY-KON-F=" KEY-KON-F.
*    DISPLAY "KEY-KEP-F=" KEY-KEP-F.
*T↑
*
*ＫＥＹ比較
*        紐付ＫＥＹ <　欠品ＫＥＹ
     IF  KEY-KON-F  <  KEY-KEP-F
*T↓
*    DISPLAY "KEY-KON-F < KEY-KON-F"
*T↑
*
*        発注ＭＳＧ検索
         MOVE         KON-F01    TO    HAC-F011
         MOVE         KON-F02    TO    HAC-F012
         MOVE         KON-F03    TO    HAC-F013
         MOVE         KON-F04    TO    HAC-F02
         MOVE         KON-F07    TO    HAC-F346
         MOVE         KON-F08    TO    HAC-F308
         MOVE         KON-F10    TO    HAC-F302
         MOVE         KON-F11    TO    HAC-F402
         PERFORM      BMSHACL3-READ-SEC
         IF           HAC-INV  = "INV"
                      DISPLAY NC"発注データが存在しません！"
                                                      UPON CONS
                      MOVE       "4010"  TO   PROGRAM-STATUS
                      STOP     RUN
         END-IF
*        カインズ出荷データ検索
         MOVE         KON-F01    TO    SYK-F011
         MOVE         KON-F02    TO    SYK-F012
         MOVE         KON-F03    TO    SYK-F013
         MOVE         KON-F04    TO    SYK-F02
         MOVE         KON-F07    TO    SYK-F349
         MOVE         KON-F05    TO    SYK-F348
         MOVE         KON-F06    TO    SYK-F305
         MOVE         KON-F08    TO    SYK-F309
         MOVE         KON-F10    TO    SYK-F302
         MOVE         KON-F11    TO    SYK-F402
         PERFORM      CZSYKTC6-READ-SEC
         IF           SYK-INV  = "INV"
                      DISPLAY NC"出荷データが存在しません！１"
                                                     UPON CONS
                      DISPLAY "SYK-F011 = " SYK-F011   UPON CONS
                      DISPLAY "SYK-F012 = " SYK-F012   UPON CONS
                      DISPLAY "SYK-F013 = " SYK-F013   UPON CONS
                      DISPLAY "SYK-F02  = " SYK-F02    UPON CONS
                      DISPLAY "SYK-F349 = " SYK-F349   UPON CONS
                      DISPLAY "SYK-F348 = " SYK-F348   UPON CONS
                      DISPLAY "SYK-F305 = " SYK-F305   UPON CONS
                      DISPLAY "SYK-F309 = " SYK-F309   UPON CONS
                      DISPLAY "SYK-F302 = " SYK-F302   UPON CONS
                      DISPLAY "SYK-F402 = " SYK-F402   UPON CONS
                      MOVE       "4010"  TO   PROGRAM-STATUS
                      STOP     RUN
         END-IF
*
         IF  KEY-KON-F  <  KEY-SND-F
             DISPLAY NC"＃　データ不整合！　＃" UPON CONS
             MOVE    "4010"        TO        PROGRAM-STATUS
             STOP    RUN
         END-IF
*
         IF  KEY-KON-F  =  KEY-SND-F
*
             IF  ( KEY-CDATE-KON-F05  NOT = KEY-CDATE-SND ) OR
                 ( KEY-CCODE-KON-F06  NOT = KEY-CCODE-SND )
*                  出荷梱包リスト（Ｂ）出力
                   MOVE     "KON"     TO    CTL-FLG
                   PERFORM  MSG-OUT-B-SEC
             END-IF
*
             IF  ( KEY-NCODE-KON-F08  NOT = KEY-NCODE-SND )
*                  発注元別出荷梱包リスト（Ｃ）出力
                   MOVE     "KON"     TO    CTL-FLG
                   PERFORM  MSG-OUT-C-SEC
             END-IF
*
             IF  ( KEY-KONNO-KON-F09  NOT = KEY-KONNO-SND )
*                  出荷梱包内容（Ｄ）出力
                   PERFORM  MSG-OUT-D-SEC
             END-IF
*
*            取引明細（Ｅ）出力
             PERFORM  MSG-OUT-E-SEC
         END-IF
*
         IF  KEY-KON-F  >  KEY-SND-F
*
             IF  ( KEY-CDATE-KON-F05  NOT = KEY-CDATE-SND ) OR
                 ( KEY-CCODE-KON-F06  NOT = KEY-CCODE-SND )
*                  出荷梱包リスト（Ｂ）出力
                   MOVE     "KON"     TO    CTL-FLG
                   PERFORM  MSG-OUT-B-SEC
             END-IF
*
             IF  ( KEY-NCODE-KON-F08  NOT = KEY-NCODE-SND )
*                  発注元別出荷梱包リスト（Ｃ）出力
                   MOVE     "KON"     TO    CTL-FLG
                   PERFORM  MSG-OUT-C-SEC
             END-IF
*
             IF  ( KEY-KONNO-KON-F09  NOT = KEY-KONNO-SND )
*                  出荷梱包内容（Ｄ）出力
                   PERFORM  MSG-OUT-D-SEC
             END-IF
*
*            取引明細（Ｅ）出力
             PERFORM  MSG-OUT-E-SEC
*
         END-IF
*
*        発注ＭＳＧ更新
         PERFORM      BMSHACL3-UPDT-SEC
*
*        カインズ出荷データ更新
         PERFORM      CZSYKTC6-UPDT-SEC
*
*        データ作成ＫＥＹ保管
         MOVE     KON-F01         TO       KEY-BDATE-SND
         MOVE     KON-F02         TO       KEY-BTIME-SND
         MOVE     KON-F03         TO       KEY-BTORI-SND
         MOVE     KON-F04         TO       KEY-SOKCD-SND
         MOVE     KON-F07         TO       KEY-NDATE-SND
         MOVE     KON-F05         TO       KEY-CDATE-SND
         MOVE     KON-F06         TO       KEY-CCODE-SND
         MOVE     KON-F08         TO       KEY-NCODE-SND
         MOVE     KON-F09         TO       KEY-KONNO-SND
         MOVE     KON-F10         TO       KEY-DENNO-SND
         MOVE     KON-F11         TO       KEY-GYONO-SND
*
*        検品紐付情報ＲＥＡＤ
         PERFORM  CZKONXX2-READ-SEC
*
*        紐付情報ＫＥＹ保管
         IF       END-FLG1    =       "HIT"
                  MOVE     KON-F01         TO   KEY-BDATE-KON-F01
                                                NEW-BDATE-KON-F01
                  MOVE     KON-F02         TO   KEY-BTIME-KON-F02
                                                NEW-BTIME-KON-F02
                  MOVE     KON-F03         TO   KEY-BTORI-KON-F03
                                                NEW-BTORI-KON-F03
                  MOVE     KON-F04         TO   KEY-SOKCD-KON-F04
                                                NEW-SOKCD-KON-F04
                  MOVE     KON-F07         TO   KEY-NDATE-KON-F07
                                                NEW-NDATE-KON-F07
                  MOVE     KON-F05         TO   KEY-CDATE-KON-F05
                                                NEW-CDATE-KON-F05
                  MOVE     KON-F06         TO   KEY-CCODE-KON-F06
                                                NEW-CCODE-KON-F06
                  MOVE     KON-F08         TO   KEY-NCODE-KON-F08
                                                NEW-NCODE-KON-F08
                  MOVE     KON-F09         TO   KEY-KONNO-KON-F09
                                                NEW-KONNO-KON-F09
                  MOVE     KON-F10         TO   KEY-DENNO-KON-F10
                                                NEW-DENNO-KON-F10
                  MOVE     KON-F11         TO   KEY-GYONO-KON-F11
                                                NEW-GYONO-KON-F11
         END-IF
*
         GO       TO       MAIN-EXIT
*
     END-IF.
*----------------------------------------------------------------
 MAIN-02.
*ＫＥＹ比較
*        紐付ＫＥＹ >　欠品ＫＥＹ
     IF  KEY-KON-F  >  KEY-KEP-F
*T↓
*    DISPLAY "KEY-KON-F > KEY-KON-F"
*T↑
*
*        発注ＭＳＧ検索
         MOVE         KEP-F011   TO    HAC-F011
         MOVE         KEP-F012   TO    HAC-F012
         MOVE         KEP-F013   TO    HAC-F013
         MOVE         KEP-F02    TO    HAC-F02
         MOVE         KEP-F349   TO    HAC-F346
         MOVE         KEP-F309   TO    HAC-F308
         MOVE         KEP-F302   TO    HAC-F302
         MOVE         KEP-F402   TO    HAC-F402
         PERFORM      BMSHACL3-READ-SEC
         IF           HAC-INV  = "INV"
                      DISPLAY NC"発注データが存在しません！"
                                                      UPON CONS
                      MOVE       "4010"  TO   PROGRAM-STATUS
                      STOP     RUN
         END-IF
*        カインズ出荷データ検索
         MOVE         KEP-F011   TO    SYK-F011
         MOVE         KEP-F012   TO    SYK-F012
         MOVE         KEP-F013   TO    SYK-F013
         MOVE         KEP-F02    TO    SYK-F02
         MOVE         KEP-F349   TO    SYK-F349
         MOVE         KEP-F348   TO    SYK-F348
         MOVE         KEP-F305   TO    SYK-F305
         MOVE         KEP-F309   TO    SYK-F309
         MOVE         KEP-F302   TO    SYK-F302
         MOVE         KEP-F402   TO    SYK-F402
         PERFORM      CZSYKTC6-READ-SEC
         IF           SYK-INV  = "INV"
                      DISPLAY NC"出荷データが存在しません！２"
                                                     UPON CONS
                      MOVE       "4010"  TO   PROGRAM-STATUS
                      STOP     RUN
         END-IF
*
         IF  KEY-KEP-F  <  KEY-SND-F
             DISPLAY NC"＃　データ不整合！　＃" UPON CONS
             MOVE    "4010"        TO        PROGRAM-STATUS
             STOP    RUN
         END-IF
*
         IF  KEY-KEP-F  =  KEY-SND-F
*
             IF  ( KEY-CDATE-KEP-F348 NOT = KEY-CDATE-SND ) OR
                 ( KEY-CCODE-KEP-F305 NOT = KEY-CCODE-SND )
*                  出荷梱包リスト（Ｂ）出力
                   MOVE     "KEP"     TO    CTL-FLG
                   PERFORM  MSG-OUT-B-SEC
             END-IF
*
             IF  ( KEY-NCODE-KEP-F309 NOT = KEY-NCODE-SND )
*                  発注元別出荷梱包リスト（Ｃ）出力
                   MOVE     "KEP"     TO    CTL-FLG
                   PERFORM  MSG-OUT-C-SEC
             END-IF
*
*            欠品情報（Ｇ）出力
             PERFORM  MSG-OUT-G-SEC
         END-IF
*
         IF  KEY-KEP-F  >  KEY-SND-F
*
             IF  ( KEY-CDATE-KEP-F348 NOT = KEY-CDATE-SND ) OR
                 ( KEY-CCODE-KEP-F305 NOT = KEY-CCODE-SND )
*                  出荷梱包リスト（Ｂ）出力
                   MOVE     "KEP"     TO    CTL-FLG
                   PERFORM  MSG-OUT-B-SEC
             END-IF
*
             IF  ( KEY-NCODE-KEP-F309 NOT = KEY-NCODE-SND )
*                  発注元別出荷梱包リスト（Ｃ）出力
                   MOVE     "KEP"     TO    CTL-FLG
                   PERFORM  MSG-OUT-C-SEC
             END-IF
*
*            欠品情報（Ｇ）出力
             PERFORM  MSG-OUT-G-SEC
*
         END-IF
*
*        カインズ欠品データ更新
         PERFORM      CZKEPTC1-UPDT-SEC
*
*        発注ＭＳＧ更新
         PERFORM      BMSHACL3-UPDT-SEC
*
*        カインズ出荷データ更新
         PERFORM      CZSYKTC6-UPDT-SEC
*
*        データ作成ＫＥＹ保管
         MOVE     KEP-F011        TO       KEY-BDATE-SND
         MOVE     KEP-F012        TO       KEY-BTIME-SND
         MOVE     KEP-F013        TO       KEY-BTORI-SND
         MOVE     KEP-F02         TO       KEY-SOKCD-SND
         MOVE     KEP-F349        TO       KEY-NDATE-SND
         MOVE     KEP-F348        TO       KEY-CDATE-SND
         MOVE     KEP-F305(1:5)   TO       KEY-CCODE-SND
         MOVE     KEP-F309(1:5)   TO       KEY-NCODE-SND
         MOVE     ALL "9"         TO       KEY-KONNO-SND
         MOVE     KEP-F302        TO       KEY-DENNO-SND
         MOVE     KEP-F402        TO       KEY-GYONO-SND
*
*        カインズ欠品データＲＥＡＤ
         PERFORM  CZKEPTC1-READ-SEC
*
*        欠品データＫＥＹ保管
         IF       END-FLG2    =       "HIT"
                  MOVE     KEP-F011        TO   KEY-BDATE-KEP-F011
                                                NEW-BDATE-KEP-F011
                  MOVE     KEP-F012        TO   KEY-BTIME-KEP-F012
                                                NEW-BTIME-KEP-F012
                  MOVE     KEP-F013        TO   KEY-BTORI-KEP-F013
                                                NEW-BTORI-KEP-F013
                  MOVE     KEP-F02         TO   KEY-SOKCD-KEP-F02
                                                NEW-SOKCD-KEP-F02
                  MOVE     KEP-F349        TO   KEY-NDATE-KEP-F349
                                                NEW-NDATE-KEP-F349
                  MOVE     KEP-F348        TO   KEY-CDATE-KEP-F348
                                                NEW-CDATE-KEP-F348
                  MOVE     KEP-F305(1:5)   TO   KEY-CCODE-KEP-F305
                                                NEW-CCODE-KEP-F305
                  MOVE     KEP-F309(1:5)   TO   KEY-NCODE-KEP-F309
                                                NEW-NCODE-KEP-F309
                  MOVE     ALL "9"         TO   KEY-KONNO-KEP-FXXX
                                                NEW-KONNO-KEP-FXXX
                  MOVE     KEP-F302        TO   KEY-DENNO-KEP-F302
                                                NEW-DENNO-KEP-F302
                  MOVE     KEP-F402        TO   KEY-GYONO-KEP-F402
                                                NEW-GYONO-KEP-F402
         END-IF
*
         GO       TO       MAIN-EXIT
*
     END-IF.
*----------------------------------------------------------------
 MAIN-03.
*ＫＥＹ比較
*        紐付ＫＥＹ =　欠品ＫＥＹ
     IF  KEY-KON-F  =  KEY-KEP-F
*T↓
*    DISPLAY "KEY-KON-F = KEY-KON-F"
*T↑
*
         PERFORM UNTIL (( KON-F01  NOT =  KEY-BDATE-KON-F01 )  OR
                        ( KON-F02  NOT =  KEY-BTIME-KON-F02 )  OR
                        ( KON-F03  NOT =  KEY-BTORI-KON-F03 )  OR
                        ( KON-F04  NOT =  KEY-SOKCD-KON-F04 )  OR
                        ( KON-F07  NOT =  KEY-NDATE-KON-F07 )  OR
                        ( KON-F05  NOT =  KEY-CDATE-KON-F05 )  OR
                        ( KON-F06  NOT =  KEY-CCODE-KON-F06 )  OR
                        ( KON-F08  NOT =  KEY-NCODE-KON-F08 )) OR
                        ( END-FLG1     =  "END"             )
*
*            発注ＭＳＧ検索
             MOVE         KON-F01    TO    HAC-F011
             MOVE         KON-F02    TO    HAC-F012
             MOVE         KON-F03    TO    HAC-F013
             MOVE         KON-F04    TO    HAC-F02
             MOVE         KON-F07    TO    HAC-F346
             MOVE         KON-F08    TO    HAC-F308
             MOVE         KON-F10    TO    HAC-F302
             MOVE         KON-F11    TO    HAC-F402
             PERFORM      BMSHACL3-READ-SEC
             IF           HAC-INV  = "INV"
                          DISPLAY NC"発注データが存在しません！"
                                                      UPON CONS
                          MOVE       "4010"  TO   PROGRAM-STATUS
                          STOP     RUN
             END-IF
*            カインズ出荷データ検索
             MOVE         KON-F01    TO    SYK-F011
             MOVE         KON-F02    TO    SYK-F012
             MOVE         KON-F03    TO    SYK-F013
             MOVE         KON-F04    TO    SYK-F02
             MOVE         KON-F07    TO    SYK-F349
             MOVE         KON-F05    TO    SYK-F348
             MOVE         KON-F06    TO    SYK-F305
             MOVE         KON-F08    TO    SYK-F309
             MOVE         KON-F10    TO    SYK-F302
             MOVE         KON-F11    TO    SYK-F402
             PERFORM      CZSYKTC6-READ-SEC
             IF           SYK-INV  = "INV"
                      DISPLAY NC"出荷データが存在しません！３"
                                                     UPON CONS
                          MOVE       "4010"  TO   PROGRAM-STATUS
                          STOP     RUN
             END-IF
*
             IF  NEW-KON-F  <  KEY-SND-F
                 DISPLAY NC"＃　データ不整合！　＃" UPON CONS
                 MOVE    "4010"        TO        PROGRAM-STATUS
                 STOP    RUN
             END-IF
*
             IF  NEW-KON-F  =  KEY-SND-F
*
                 IF  ( NEW-CDATE-KON-F05  NOT = KEY-CDATE-SND )
                    OR
                     ( NEW-CCODE-KON-F06  NOT = KEY-CCODE-SND )
*                      出荷梱包リスト（Ｂ）出力
                       MOVE     "KON"     TO    CTL-FLG
                       PERFORM  MSG-OUT-B-SEC
                 END-IF
*
                 IF  ( NEW-NCODE-KON-F08  NOT = KEY-NCODE-SND )
*                      発注元別出荷梱包リスト（Ｃ）出力
                       MOVE     "KON"     TO    CTL-FLG
                       PERFORM  MSG-OUT-C-SEC
                 END-IF
*
                 IF  ( NEW-KONNO-KON-F09  NOT = KEY-KONNO-SND )
*                      出荷梱包内容（Ｄ）出力
                       PERFORM  MSG-OUT-D-SEC
                 END-IF
*
*                取引明細（Ｅ）出力
                 PERFORM  MSG-OUT-E-SEC
             END-IF
*
             IF  NEW-KON-F  >  KEY-SND-F
*
                 IF  ( NEW-CDATE-KON-F05  NOT = KEY-CDATE-SND )
                    OR
                     ( NEW-CCODE-KON-F06  NOT = KEY-CCODE-SND )
*                      出荷梱包リスト（Ｂ）出力
                       MOVE     "KON"     TO    CTL-FLG
                       PERFORM  MSG-OUT-B-SEC
                 END-IF
*
                 IF  ( NEW-NCODE-KON-F08  NOT = KEY-NCODE-SND )
*                      発注元別出荷梱包リスト（Ｃ）出力
                       MOVE     "KON"     TO    CTL-FLG
                       PERFORM  MSG-OUT-C-SEC
                 END-IF
*
                 IF  ( NEW-KONNO-KON-F09  NOT = KEY-KONNO-SND )
*                      出荷梱包内容（Ｄ）出力
                       PERFORM  MSG-OUT-D-SEC
                 END-IF
*
*                取引明細（Ｅ）出力
                 PERFORM  MSG-OUT-E-SEC
*
             END-IF
*
*            発注ＭＳＧ更新
             PERFORM      BMSHACL3-UPDT-SEC
*
*            カインズ出荷データ更新
             PERFORM      CZSYKTC6-UPDT-SEC
*
*            データ作成ＫＥＹ保管
             MOVE     KON-F01         TO       KEY-BDATE-SND
             MOVE     KON-F02         TO       KEY-BTIME-SND
             MOVE     KON-F03         TO       KEY-BTORI-SND
             MOVE     KON-F04         TO       KEY-SOKCD-SND
             MOVE     KON-F07         TO       KEY-NDATE-SND
             MOVE     KON-F05         TO       KEY-CDATE-SND
             MOVE     KON-F06         TO       KEY-CCODE-SND
             MOVE     KON-F08         TO       KEY-NCODE-SND
             MOVE     KON-F09         TO       KEY-KONNO-SND
             MOVE     KON-F10         TO       KEY-DENNO-SND
             MOVE     KON-F11         TO       KEY-GYONO-SND
*
*            検品紐付情報ＲＥＡＤ
             PERFORM  CZKONXX2-READ-SEC
*            最新ＫＥＹ保管
             MOVE     KON-F01         TO       NEW-BDATE-KON-F01
             MOVE     KON-F02         TO       NEW-BTIME-KON-F02
             MOVE     KON-F03         TO       NEW-BTORI-KON-F03
             MOVE     KON-F04         TO       NEW-SOKCD-KON-F04
             MOVE     KON-F07         TO       NEW-NDATE-KON-F07
             MOVE     KON-F05         TO       NEW-CDATE-KON-F05
             MOVE     KON-F06         TO       NEW-CCODE-KON-F06
             MOVE     KON-F08         TO       NEW-NCODE-KON-F08
             MOVE     KON-F09         TO       NEW-KONNO-KON-F09
             MOVE     KON-F10         TO       NEW-DENNO-KON-F10
             MOVE     KON-F11         TO       NEW-GYONO-KON-F11
*
*
*            紐付情報ＫＥＹ保管
*            IF       END-FLG1    =    "HIT"
*                 MOVE     KON-F01     TO   KEY-BDATE-KON-F01
*                 MOVE     KON-F02     TO   KEY-BTIME-KON-F02
*                 MOVE     KON-F03     TO   KEY-BTORI-KON-F03
*                 MOVE     KON-F04     TO   KEY-SOKCD-KON-F04
*                 MOVE     KON-F07     TO   KEY-NDATE-KON-F07
*                 MOVE     KON-F05     TO   KEY-CDATE-KON-F05
*                 MOVE     KON-F06     TO   KEY-CCODE-KON-F06
*                 MOVE     KON-F08     TO   KEY-NCODE-KON-F08
*                 MOVE     KON-F09     TO   KEY-KONNO-KON-F09
*                 MOVE     KON-F10     TO   KEY-DENNO-KON-F10
*                 MOVE     KON-F11     TO   KEY-GYONO-KON-F11
*        END-IF
*
         END-PERFORM
*        -------------------------------------------------------
         PERFORM UNTIL
                 (( KEP-F011      NOT =  KEY-BDATE-KEP-F011 )  OR
                  ( KEP-F012      NOT =  KEY-BTIME-KEP-F012 )  OR
                  ( KEP-F013      NOT =  KEY-BTORI-KEP-F013 )  OR
                  ( KEP-F02       NOT =  KEY-SOKCD-KEP-F02  )  OR
                  ( KEP-F349      NOT =  KEY-NDATE-KEP-F349 )  OR
                  ( KEP-F348      NOT =  KEY-CDATE-KEP-F348 )  OR
                  ( KEP-F305(1:5) NOT =  KEY-CCODE-KEP-F305 )  OR
                  ( KEP-F309(1:5) NOT =  KEY-NCODE-KEP-F309 )) OR
                  ( END-FLG2          =  "END"              )
*
*            発注ＭＳＧ検索
             MOVE         KEP-F011   TO    HAC-F011
             MOVE         KEP-F012   TO    HAC-F012
             MOVE         KEP-F013   TO    HAC-F013
             MOVE         KEP-F02    TO    HAC-F02
             MOVE         KEP-F349   TO    HAC-F346
             MOVE         KEP-F309   TO    HAC-F308
             MOVE         KEP-F302   TO    HAC-F302
             MOVE         KEP-F402   TO    HAC-F402
             PERFORM      BMSHACL3-READ-SEC
             IF           HAC-INV  = "INV"
                          DISPLAY NC"発注データが存在しません！"
                                                      UPON CONS
                          MOVE       "4010"  TO   PROGRAM-STATUS
                          STOP     RUN
             END-IF
*            カインズ出荷データ検索
             MOVE         KEP-F011   TO    SYK-F011
             MOVE         KEP-F012   TO    SYK-F012
             MOVE         KEP-F013   TO    SYK-F013
             MOVE         KEP-F02    TO    SYK-F02
             MOVE         KEP-F349   TO    SYK-F349
             MOVE         KEP-F348   TO    SYK-F348
             MOVE         KEP-F305   TO    SYK-F305
             MOVE         KEP-F309   TO    SYK-F309
             MOVE         KEP-F302   TO    SYK-F302
             MOVE         KEP-F402   TO    SYK-F402
             PERFORM      CZSYKTC6-READ-SEC
             IF           SYK-INV  = "INV"
                      DISPLAY NC"出荷データが存在しません！４"
                                                     UPON CONS
                          MOVE       "4010"  TO   PROGRAM-STATUS
                          STOP     RUN
             END-IF
*
*            欠品情報（Ｇ）出力
             PERFORM  MSG-OUT-G-SEC
*
*            カインズ欠品データ更新
             PERFORM  CZKEPTC1-UPDT-SEC
*
*            発注ＭＳＧ更新
             PERFORM      BMSHACL3-UPDT-SEC
*
*            カインズ出荷データ更新
             PERFORM      CZSYKTC6-UPDT-SEC
*
*            データ作成ＫＥＹ保管
             MOVE     KEP-F011        TO       KEY-BDATE-SND
             MOVE     KEP-F012        TO       KEY-BTIME-SND
             MOVE     KEP-F013        TO       KEY-BTORI-SND
             MOVE     KEP-F02         TO       KEY-SOKCD-SND
             MOVE     KEP-F349        TO       KEY-NDATE-SND
             MOVE     KEP-F348        TO       KEY-CDATE-SND
             MOVE     KEP-F305(1:5)   TO       KEY-CCODE-SND
             MOVE     KEP-F309(1:5)   TO       KEY-NCODE-SND
             MOVE     ALL "9"         TO       KEY-KONNO-SND
             MOVE     KEP-F302        TO       KEY-DENNO-SND
             MOVE     KEP-F402        TO       KEY-GYONO-SND
*
*            カインズ欠品データＲＥＡＤ
             PERFORM  CZKEPTC1-READ-SEC
*            最新ＫＥＹ保管
             MOVE     KEP-F011        TO       NEW-BDATE-KEP-F011
             MOVE     KEP-F012        TO       NEW-BTIME-KEP-F012
             MOVE     KEP-F013        TO       NEW-BTORI-KEP-F013
             MOVE     KEP-F02         TO       NEW-SOKCD-KEP-F02
             MOVE     KEP-F349        TO       NEW-NDATE-KEP-F349
             MOVE     KEP-F348        TO       NEW-CDATE-KEP-F348
             MOVE     KEP-F305(1:5)   TO       NEW-CCODE-KEP-F305
             MOVE     KEP-F309(1:5)   TO       NEW-NCODE-KEP-F309
             MOVE     ALL "9"         TO       NEW-KONNO-KEP-FXXX
             MOVE     KEP-F302        TO       NEW-DENNO-KEP-F302
             MOVE     KEP-F402        TO       NEW-GYONO-KEP-F402
*
*            欠品データＫＥＹ保管
*            IF       END-FLG2    =    "HIT"
*                 MOVE     KEP-F011      TO   KEY-BDATE-KEP-F011
*                 MOVE     KEP-F012      TO   KEY-BTIME-KEP-F012
*                 MOVE     KEP-F013      TO   KEY-BTORI-KEP-F013
*                 MOVE     KEP-F02       TO   KEY-SOKCD-KEP-F02
*                 MOVE     KEP-F349      TO   KEY-NDATE-KEP-F349
*                 MOVE     KEP-F348      TO   KEY-CDATE-KEP-F348
*                 MOVE     KEP-F305(1:5) TO   KEY-CCODE-KEP-F305
*                 MOVE     KEP-F309(1:5) TO   KEY-NCODE-KEP-F309
*                 MOVE     ALL "9"       TO   KEY-KONNO-KEP-FXXX
*                 MOVE     KEP-F302      TO   KEY-DENNO-KEP-F302
*                 MOVE     KEP-F402      TO   KEY-GYONO-KEP-F402
*        END-IF
*
         END-PERFORM
*        -------------------------------------------------------
*        紐付情報ＫＥＹ保管
         IF       END-FLG1    =    "HIT"
                  MOVE     KON-F01     TO   KEY-BDATE-KON-F01
                  MOVE     KON-F02     TO   KEY-BTIME-KON-F02
                  MOVE     KON-F03     TO   KEY-BTORI-KON-F03
                  MOVE     KON-F04     TO   KEY-SOKCD-KON-F04
                  MOVE     KON-F07     TO   KEY-NDATE-KON-F07
                  MOVE     KON-F05     TO   KEY-CDATE-KON-F05
                  MOVE     KON-F06     TO   KEY-CCODE-KON-F06
                  MOVE     KON-F08     TO   KEY-NCODE-KON-F08
                  MOVE     KON-F09     TO   KEY-KONNO-KON-F09
                  MOVE     KON-F10     TO   KEY-DENNO-KON-F10
                  MOVE     KON-F11     TO   KEY-GYONO-KON-F11
         END-IF
*
*        欠品データＫＥＹ保管
         IF       END-FLG2    =    "HIT"
                  MOVE     KEP-F011      TO   KEY-BDATE-KEP-F011
                  MOVE     KEP-F012      TO   KEY-BTIME-KEP-F012
                  MOVE     KEP-F013      TO   KEY-BTORI-KEP-F013
                  MOVE     KEP-F02       TO   KEY-SOKCD-KEP-F02
                  MOVE     KEP-F349      TO   KEY-NDATE-KEP-F349
                  MOVE     KEP-F348      TO   KEY-CDATE-KEP-F348
                  MOVE     KEP-F305(1:5) TO   KEY-CCODE-KEP-F305
                  MOVE     KEP-F309(1:5) TO   KEY-NCODE-KEP-F309
                  MOVE     ALL "9"       TO   KEY-KONNO-KEP-FXXX
                  MOVE     KEP-F302      TO   KEY-DENNO-KEP-F302
                  MOVE     KEP-F402      TO   KEY-GYONO-KEP-F402
         END-IF
*
         GO       TO       MAIN-EXIT
*
     END-IF.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*              終了処理                                        *
******************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"       TO     S-NAME.
*
*ファイルクローズ
     CLOSE     CZKONXX2 CZKEPTC1 CZSYKTC6 BMSHACL3 WKONCZXX.
*
     MOVE      CNT-WRT-ALL    TO     PARA-OUT-KENSU.
*
     DISPLAY   "出荷明細数＝"  CNT-WRT-E  UPON CONS.
     DISPLAY   "欠品明細数＝"  CNT-WRT-G  UPON CONS.
     DISPLAY   MSG-END                    UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*
******************************************************************
*            検品紐付情報（カインズ）ＳＴＡＲＴ
******************************************************************
 CZKONXX2-START-SEC          SECTION.
*
     MOVE    "CZKONXX2-START-SEC"         TO   S-NAME.
*
 CZKONXX2-START-100.
*
     MOVE     SPACE                       TO   KON-REC.
     INITIALIZE                                KON-REC.
     MOVE     PARA-IN-BDATE               TO   KON-F01.
     MOVE     PARA-IN-BTIME               TO   KON-F02.
     MOVE     PARA-IN-BTORI               TO   KON-F03.
     MOVE     PARA-IN-SOKCD               TO   KON-F04.
     MOVE     PARA-IN-NDATE               TO   KON-F07.
*
     START    CZKONXX2       KEY    IS     >=  KON-F01
                                               KON-F02
                                               KON-F03
                                               KON-F04
                                               KON-F07
                                               KON-F05
                                               KON-F06
                                               KON-F08
                                               KON-F09
                                               KON-F10
                                               KON-F11
              INVALID
                     MOVE  "END"   TO          END-FLG1
                     GO            TO          CZKONXX2-START-EXIT
     END-START.
*
 CZKONXX2-START-EXIT.
     EXIT.
*
******************************************************************
*            検品紐付情報（カインズ）読込
******************************************************************
 CZKONXX2-READ-SEC           SECTION.
*
     MOVE    "CZKONXX2-READ-SEC"          TO   S-NAME.
*
 CZKONXX2-READ-100.
*
     READ     CZKONXX2
              AT  END       MOVE  "END"        TO  END-FLG1
                            MOVE   HIGH-VALUE  TO  KEY-KON
                            GO     TO     CZKONXX2-READ-EXIT
              NOT AT  END   ADD    1      TO   RD-CNT1
     END-READ.
*
*件数表示
     IF       RD-CNT1(6:3)   =  "000"  OR  "500"
              DISPLAY "# READ-CNT = " RD-CNT1
              UPON CONS
     END-IF.
* 範囲判定
     IF   ( KON-F01   =  PARA-IN-BDATE ) AND
          ( KON-F02   =  PARA-IN-BTIME ) AND
          ( KON-F03   =  PARA-IN-BTORI ) AND
          ( KON-F04   =  PARA-IN-SOKCD ) AND
          ( KON-F07   =  PARA-IN-NDATE )
          CONTINUE
     ELSE
          MOVE   "END"   TO   END-FLG1
          GO             TO   CZKONXX2-READ-EXIT
     END-IF.
*
* 発注ＭＳＧ存在チェック
     MOVE     KON-F01    TO    HAC-F011.
     MOVE     KON-F02    TO    HAC-F012.
     MOVE     KON-F03    TO    HAC-F013.
     MOVE     KON-F04    TO    HAC-F02.
     MOVE     KON-F07    TO    HAC-F346.
     MOVE     KON-F08    TO    HAC-F308.
     MOVE     KON-F10    TO    HAC-F302.
     MOVE     KON-F11    TO    HAC-F402.
     PERFORM  BMSHACL3-READ-SEC.
     IF       HAC-INV  = "INV"
              DISPLAY NC"発注データが存在しません！" UPON CONS
              MOVE       "4010"  TO   PROGRAM-STATUS
              STOP     RUN
     END-IF.
* 出荷ＭＳＧ対象チェック
     MOVE     KON-F01    TO    SYK-F011.
     MOVE     KON-F02    TO    SYK-F012.
     MOVE     KON-F03    TO    SYK-F013.
     MOVE     KON-F04    TO    SYK-F02.
     MOVE     KON-F07    TO    SYK-F349.
     MOVE     KON-F05    TO    SYK-F348.
     MOVE     KON-F06    TO    SYK-F305.
     MOVE     KON-F08    TO    SYK-F309.
     MOVE     KON-F10    TO    SYK-F302.
     MOVE     KON-F11    TO    SYK-F402.
     PERFORM  CZSYKTC6-READ-SEC.
     IF       SYK-INV  = "INV"
              DISPLAY NC"出荷データが存在しません！５" UPON CONS
              MOVE       "4010"  TO   PROGRAM-STATUS
              STOP     RUN
     END-IF.
*    出荷送信済フラグ判定
     IF   SYK-F601  =  " "
          MOVE      "HIT"   TO   END-FLG1
     ELSE
          IF  ( SYK-F602  =  SYS-DATEW  )   AND
              ( SYK-F603  =  SYS-HHMMSS )
                MOVE      "HIT"   TO   END-FLG1
          ELSE
                GO  TO    CZKONXX2-READ-100
          END-IF
     END-IF.
*
 CZKONXX2-READ-EXIT.
     EXIT.
*
******************************************************************
*            カインズ欠品データＳＴＡＲＴ
******************************************************************
 CZKEPTC1-START-SEC          SECTION.
*
     MOVE    "CZKEPTC1-START-SEC"         TO   S-NAME.
*
 CZKEPTC1-START-100.
*
     MOVE     SPACE                       TO   KEP-REC.
     INITIALIZE                                KEP-REC.
     MOVE     PARA-IN-BDATE               TO   KEP-F011.
     MOVE     PARA-IN-BTIME               TO   KEP-F012.
     MOVE     PARA-IN-BTORI               TO   KEP-F013.
     MOVE     PARA-IN-SOKCD               TO   KEP-F02.
     MOVE     PARA-IN-NDATE               TO   KEP-F349.
*
     START    CZKEPTC1      KEY    IS     >=   KEP-F011
                                               KEP-F012
                                               KEP-F013
                                               KEP-F02
                                               KEP-F349
                                               KEP-F348
                                               KEP-F305
                                               KEP-F309
                                               KEP-F302
                                               KEP-F402
              INVALID
                     MOVE  "END"   TO          END-FLG2
                     GO            TO          CZKEPTC1-START-EXIT
     END-START.
*
 CZKEPTC1-START-EXIT.
     EXIT.
*
******************************************************************
*            カインズ欠品データ読込
******************************************************************
 CZKEPTC1-READ-SEC           SECTION.
*
     MOVE    "CZKEPTC1-READ-SEC"          TO   S-NAME.
*
 CZKEPTC1-READ-100.
*
     READ     CZKEPTC1
              AT  END       MOVE  "END"        TO  END-FLG2
                            MOVE   HIGH-VALUE  TO  KEY-KEP
                            GO     TO     CZKEPTC1-READ-EXIT
              NOT AT  END   ADD    1      TO   RD-CNT2
     END-READ.
*
*件数表示
     IF       RD-CNT1(6:3)   =  "000"  OR  "500"
              DISPLAY "# READ-CNT = " RD-CNT2
              UPON CONS
     END-IF.
* 範囲判定
     IF   ( KEP-F011  =  PARA-IN-BDATE ) AND
          ( KEP-F012  =  PARA-IN-BTIME ) AND
          ( KEP-F013  =  PARA-IN-BTORI ) AND
          ( KEP-F02   =  PARA-IN-SOKCD ) AND
          ( KEP-F349  =  PARA-IN-NDATE )
          MOVE   "HIT"   TO   END-FLG2
     ELSE
          MOVE   "END"   TO   END-FLG2
          GO             TO   CZKEPTC1-READ-EXIT
     END-IF.
*
 CZKEPTC1-READ-EXIT.
     EXIT.
*
******************************************************************
*            カインズ欠品データ更新
******************************************************************
 CZKEPTC1-UPDT-SEC          SECTION.
*
     MOVE    "CZKEPTC1-UPDT-SEC"          TO   S-NAME.
*
 CZKEPTC1-UPDT-100.
*
     MOVE    "1"                          TO   KEP-F601.
     MOVE     SYS-DATEW                   TO   KEP-F602.
     MOVE     SYS-HHMMSS                  TO   KEP-F603.
*
     REWRITE  KEP-REC.
*
 CZKEPTC1-UPDT-EXIT.
     EXIT.
*
******************************************************************
*            発注メッセージ存在チェック
******************************************************************
 BMSHACL3-READ-SEC           SECTION.
*
     MOVE    "BMSHACL3-READ-SEC"          TO   S-NAME.
*
 BMSHACL3-READ-100.
*
     READ     BMSHACL3
              INVALID
                     MOVE     "INV"       TO   HAC-INV
              NOT INVALID
                     MOVE     "   "       TO   HAC-INV
     END-READ.
*
 BMSHACL3-READ-EXIT.
     EXIT.
*
******************************************************************
*            発注メッセージ更新
******************************************************************
 BMSHACL3-UPDT-SEC          SECTION.
*
     MOVE    "BMSHACL3-UPDT-SEC"          TO   S-NAME.
*
 BMSHACL3-UPDT-100.
*
     MOVE    "1"                          TO   HAC-F801.
     MOVE     SYS-DATEW                   TO   HAC-F802.
     MOVE     SYS-HHMMSS                  TO   HAC-F803.
*
     REWRITE  HAC-REC.
*
 BMSHACL3-UPDT-EXIT.
     EXIT.
*
******************************************************************
*            カインズ出荷データ存在チェック
******************************************************************
 CZSYKTC6-READ-SEC           SECTION.
*
     MOVE    "CZSYKTC6-READ-SEC"          TO   S-NAME.
*
 CZSYKTC6-READ-100.
*
     READ     CZSYKTC6
              INVALID
                     MOVE     "INV"       TO   SYK-INV
              NOT INVALID
                     MOVE     "   "       TO   SYK-INV
     END-READ.
*
 CZSYKTC6-READ-EXIT.
     EXIT.
*
******************************************************************
*            カインズ出荷データ更新
******************************************************************
 CZSYKTC6-UPDT-SEC          SECTION.
*
     MOVE    "CZSYKTC6-UPDT-SEC"          TO   S-NAME.
*
 CZSYKTC6-UPDT-100.
*
     MOVE    "1"                          TO   SYK-F601.
     MOVE     SYS-DATEW                   TO   SYK-F602.
     MOVE     SYS-HHMMSS                  TO   SYK-F603.
*
     REWRITE  SYK-REC.
*
 CZSYKTC6-UPDT-EXIT.
     EXIT.
*
******************************************************************
*  　          メッセージヘッダ（Ａ）出力
******************************************************************
 MSG-OUT-A-SEC     SECTION.
*
     MOVE     "MSG-OUT-A-SEC"          TO    S-NAME.
*
     MOVE     SPACE                    TO    SNDA-REC.
     INITIALIZE                              SNDA-REC.
*
     IF       CTL-FLG    =    "KON"
              GO                       TO    MSG-OUT-A-01
     ELSE
              GO                       TO    MSG-OUT-A-02
     END-IF.
*
*紐付情報データ基点
 MSG-OUT-A-01.
*レコード区分
     MOVE     SYK-F101                 TO    SNDA-F101.
*ヘッダバージョン
     MOVE     SYK-F102                 TO    SNDA-F102.
*送信者ＩＤ
     MOVE     SYK-F103                 TO    SNDA-F103.
*送信者ＩＤ発行元
     MOVE     SYK-F104                 TO    SNDA-F104.
*受信者ＩＤ
*    MOVE     SYK-F105                 TO    SNDA-F105.
     MOVE     "89213000"               TO    SNDA-F105.
*受信者ＩＤ発行元
     MOVE     SYK-F106                 TO    SNDA-F106.
*流通ＢＭＳ名称
     MOVE     SYK-F107                 TO    SNDA-F107.
*バージョン
     MOVE     SYK-F108                 TO    SNDA-F108.
*インスタンスＩＤ    ※送信者ＩＤ６桁想定
*    送信者ID + 受信者ID + YYYYMMDDHHMMSS
*      (6)    +   (6)    +      (14)      =  26桁
     MOVE     SYK-F109                 TO    SNDA-F109.
*メッセージ種
     MOVE     "PACKAGE SHIPMENT NOTIFICATION"
                                       TO    SNDA-F110.
*複合メッセージフラグ
     MOVE     SYK-F111                 TO    SNDA-F111.
*作成日時
     MOVE     SYK-F112                 TO    SNDA-F112.
*テスト区分タイプ
     MOVE     SYK-F113                 TO    SNDA-F113.
*テスト区分インスタンスＩＤ
     MOVE     SYK-F114                 TO    SNDA-F114.
*テスト区分ＩＤ
     MOVE     SYK-F115                 TO    SNDA-F115.
*最終送信先タイプ
     MOVE     SYK-F116                 TO    SNDA-F116.
*最終送信先インスタンスＩＤ
     MOVE     SYK-F117                 TO    SNDA-F117.
*最終送信先ＩＤ
     MOVE     SYK-F118                 TO    SNDA-F118.
*メッセージ識別ＩＤ　※送信者ＩＤ６桁想定
*    "MSG-" + インスタンスＩＤ
*     (4)   +      (26)                   =  30桁
     MOVE     SYK-F119                 TO    SNDA-F119.
*送信者ステーションアドレス
     MOVE     SYK-F120                 TO    SNDA-F120.
*最終受信者ステーションアドレス
     MOVE     SYK-F121                 TO    SNDA-F121.
*直接受信者ステーションアドレス
*    MOVE     SYK-F122                 TO    SNDA-F122.
     MOVE     "89213000"               TO    SNDA-F122.
*取引件数
     MOVE     PARA-IN-MAISU            TO    SNDA-F123.
*
     GO                                TO    MSG-OUT-A-03.
*------------------
*欠品情報データ基点
 MSG-OUT-A-02.
*レコード区分
     MOVE     KEP-F101                 TO    SNDA-F101.
*ヘッダバージョン
     MOVE     KEP-F102                 TO    SNDA-F102.
*送信者ＩＤ
     MOVE     KEP-F103                 TO    SNDA-F103.
*送信者ＩＤ発行元
     MOVE     KEP-F104                 TO    SNDA-F104.
*受信者ＩＤ
*    MOVE     KEP-F105                 TO    SNDA-F105.
     MOVE     "89213000"               TO    SNDA-F105.
*受信者ＩＤ発行元
     MOVE     KEP-F106                 TO    SNDA-F106.
*流通ＢＭＳ名称
     MOVE     KEP-F107                 TO    SNDA-F107.
*バージョン
     MOVE     KEP-F108                 TO    SNDA-F108.
*インスタンスＩＤ    ※送信者ＩＤ６桁想定
*    送信者ID + 受信者ID + YYYYMMDDHHMMSS
*      (6)    +   (6)    +      (14)      =  26桁
     MOVE     KEP-F109                 TO    SNDA-F109.
*メッセージ種
     MOVE     "PACKAGE SHIPMENT NOTIFICATION"
                                       TO    SNDA-F110.
*複合メッセージフラグ
     MOVE     KEP-F111                 TO    SNDA-F111.
*作成日時
     MOVE     KEP-F112                 TO    SNDA-F112.
*テスト区分タイプ
     MOVE     KEP-F113                 TO    SNDA-F113.
*テスト区分インスタンスＩＤ
     MOVE     KEP-F114                 TO    SNDA-F114.
*テスト区分ＩＤ
     MOVE     KEP-F115                 TO    SNDA-F115.
*最終送信先タイプ
     MOVE     KEP-F116                 TO    SNDA-F116.
*最終送信先インスタンスＩＤ
     MOVE     KEP-F117                 TO    SNDA-F117.
*最終送信先ＩＤ
     MOVE     KEP-F118                 TO    SNDA-F118.
*メッセージ識別ＩＤ　※送信者ＩＤ６桁想定
*    "MSG-" + インスタンスＩＤ
*     (4)   +      (26)                   =  30桁
     MOVE     KEP-F119                 TO    SNDA-F119.
*送信者ステーションアドレス
     MOVE     KEP-F120                 TO    SNDA-F120.
*最終受信者ステーションアドレス
     MOVE     KEP-F121                 TO    SNDA-F121.
*直接受信者ステーションアドレス
*    MOVE     KEP-F122                 TO    SNDA-F122.
     MOVE     "89213000"               TO    SNDA-F122.
*取引件数
     MOVE     PARA-IN-MAISU            TO    SNDA-F123.
*
     GO                                TO    MSG-OUT-A-03.
*------------------
 MSG-OUT-A-03.
*出荷梱包ＭＳＧ出力
     MOVE     "1"                      TO    SNDA-REC(1198:1).
     WRITE    SND-REC                  FROM  SNDA-REC.
     ADD      1                        TO    CNT-WRT-ALL
                                             CNT-WRT-A.
*
 MSG-OUT-A-EXIT.
     EXIT.
******************************************************************
*  　          出荷梱包リスト（Ｂ）出力
******************************************************************
 MSG-OUT-B-SEC     SECTION.
*
     MOVE     "MSG-OUT-B-SEC"          TO    S-NAME.
*
     MOVE     SPACE                    TO    SNDB-REC.
     INITIALIZE                              SNDB-REC.
*
*  制御バイト
     MOVE     X"28"                    TO    SNDB-S2081
                                             SNDB-S2121
                                             SNDB-S2181
                                             SNDB-S2211.
     MOVE     X"29"                    TO    SNDB-S2082
                                             SNDB-S2122
                                             SNDB-S2182
                                             SNDB-S2212.
*
     IF       CTL-FLG    =    "KON"
              GO                       TO    MSG-OUT-B-01
     ELSE
              GO                       TO    MSG-OUT-B-02
     END-IF.
*
*紐付情報データ基点
 MSG-OUT-B-01.
*  レコード区分
     MOVE     "B"                      TO    SNDB-F201.
*  ＸＭＬ内容バージョンＩＤ
     MOVE     SYK-F202                 TO    SNDB-F202.
*  ＸＭＬ構造バージョンＩＤ
     MOVE     SYK-F203                 TO    SNDB-F203.
*  拡張情報ネームスペース
     MOVE     SYK-F204                 TO    SNDB-F204.
*  拡張情報バージョン番号
     MOVE     SYK-F205                 TO    SNDB-F205.
*  請求取引先コード
     MOVE     SYK-F320                 TO    SNDB-F206.
*  請求取引先GLN
     MOVE     SYK-F321                 TO    SNDB-F207.
*  請求取引先名
     MOVE     SYK-F322                 TO    SNDB-F208.
*  請求取引先名カナ
     MOVE     SYK-F323                 TO    SNDB-F209.
*  取引先コード
     MOVE     SYK-F324                 TO    SNDB-F210.
*  取引先GLN
     MOVE     SYK-F325                 TO    SNDB-F211.
*  取引先名称
     MOVE     SYK-F326                 TO    SNDB-F212.
*  取引先名称カナ
     MOVE     SYK-F327                 TO    SNDB-F213.
*  枝番
     MOVE     SYK-F328                 TO    SNDB-F214.
*  出荷先コード
     MOVE     SYK-F329                 TO    SNDB-F215.
*  直接納品先コード
     MOVE     SYK-F305                 TO    SNDB-F216.
*  直接納品先GLN
     MOVE     SYK-F306                 TO    SNDB-F217.
*  直接納品先名称
     MOVE     SYK-F307                 TO    SNDB-F218.
*  直接納品先名称カナ
     MOVE     SYK-F308                 TO    SNDB-F219.
*  出荷場所GLN
     MOVE     SYK-F330                 TO    SNDB-F220.
*  担当者
*    F221 INIT
*  担当者カナ
     MOVE     "ｴｲｷﾞｮｳﾀﾞｲﾆﾌﾞ"           TO    SNDB-F222.
*  連絡先(TEL)
     MOVE     "045-945-8816"           TO    SNDB-F223.
*  連絡先(FAX)
*    F224 INIT
*  センター納品書番号
     MOVE     SYK-F344                 TO    SNDB-F225.
*  出荷総梱包数
     MOVE     KON-F17                  TO    SNDB-F226.
*  便NO
     MOVE     SYK-F332                 TO    SNDB-F227.
*  指定納品時刻
     MOVE     SYK-F335                 TO    SNDB-F228.
*  輸送手段
     MOVE     SYK-F336                 TO    SNDB-F229.
*  取引先出荷日
     MOVE     SYK-F606                 TO    SNDB-F230.
     IF       SYK-F606                 =     ZERO
              MOVE     SPACE           TO    SNDB-REC(671:8)
     END-IF.
*  直接納品先納品日
     MOVE     SYK-F348                 TO    SNDB-F231.
     IF       SYK-F348                 =     ZERO
              MOVE     SPACE           TO    SNDB-REC(679:8)
     END-IF.
*  訂正後直接納品先納品日
     MOVE     SYK-F350                 TO    SNDB-F232.
*T
*    DISPLAY "SYK-F350=" SYK-F350 UPON CONS.
*T
     IF       SYK-F350                 =     ZERO
              MOVE     SPACE           TO    SNDB-REC(687:8)
     END-IF.
*
     GO                                TO    MSG-OUT-B-03.
*------------------
*欠品情報データ基点
 MSG-OUT-B-02.
*  レコード区分
     MOVE     "1"                      TO    SNDB-REC(1198:1).
     MOVE     "B"                      TO    SNDB-F201.
*  ＸＭＬ内容バージョンＩＤ
     MOVE     KEP-F202                 TO    SNDB-F202.
*  ＸＭＬ構造バージョンＩＤ
     MOVE     KEP-F203                 TO    SNDB-F203.
*  拡張情報ネームスペース
     MOVE     KEP-F204                 TO    SNDB-F204.
*  拡張情報バージョン番号
     MOVE     KEP-F205                 TO    SNDB-F205.
*  請求取引先コード
     MOVE     KEP-F320                 TO    SNDB-F206.
*  請求取引先GLN
     MOVE     KEP-F321                 TO    SNDB-F207.
*  請求取引先名
     MOVE     KEP-F322                 TO    SNDB-F208.
*  請求取引先名カナ
     MOVE     KEP-F323                 TO    SNDB-F209.
*  取引先コード
     MOVE     KEP-F324                 TO    SNDB-F210.
*  取引先GLN
     MOVE     KEP-F325                 TO    SNDB-F211.
*  取引先名称
     MOVE     KEP-F326                 TO    SNDB-F212.
*  取引先名称カナ
     MOVE     KEP-F327                 TO    SNDB-F213.
*  枝番
     MOVE     KEP-F328                 TO    SNDB-F214.
*  出荷先コード
     MOVE     KEP-F329                 TO    SNDB-F215.
*  直接納品先コード
     MOVE     KEP-F305                 TO    SNDB-F216.
*  直接納品先GLN
     MOVE     KEP-F306                 TO    SNDB-F217.
*  直接納品先名称
     MOVE     KEP-F307                 TO    SNDB-F218.
*  直接納品先名称カナ
     MOVE     KEP-F308                 TO    SNDB-F219.
*  出荷場所GLN
     MOVE     KEP-F330                 TO    SNDB-F220.
*  担当者
*    F221 INIT
*  担当者カナ
     MOVE     "ｴｲｷﾞｮｳﾀﾞｲﾆﾌﾞ"           TO    SNDB-F222.
*  連絡先(TEL)
     MOVE     "045-945-8816"           TO    SNDB-F223.
*  連絡先(FAX)
*    F224 INIT
*  センター納品書番号
     MOVE     KEP-F344                 TO    SNDB-F225.
*  出荷総梱包数　欠品から作成する場合はゼロ（センター分全欠品）
     MOVE     ZERO                     TO    SNDB-F226.
*  便NO
     MOVE     KEP-F332                 TO    SNDB-F227.
*  指定納品時刻
     MOVE     KEP-F335                 TO    SNDB-F228.
*  輸送手段
     MOVE     KEP-F336                 TO    SNDB-F229.
*  取引先出荷日
     MOVE     KEP-F606                 TO    SNDB-F230.
     IF       KEP-F606                 =     ZERO
              MOVE     SPACE           TO    SNDB-REC(671:8)
     END-IF.
*  直接納品先納品日
     MOVE     KEP-F348                 TO    SNDB-F231.
     IF       KEP-F348                 =     ZERO
              MOVE     SPACE           TO    SNDB-REC(679:8)
     END-IF.
*  訂正後直接納品先納品日
     MOVE     KEP-F350                 TO    SNDB-F232.
*T
*    DISPLAY "KEP-F350=" KEP-F350 UPON CONS.
*T
     IF       KEP-F350                 =     ZERO
              MOVE     SPACE           TO    SNDB-REC(687:8)
     END-IF.
*
     GO                                TO    MSG-OUT-B-03.
*------------------
 MSG-OUT-B-03.
*出荷梱包ＭＳＧ出力
     MOVE     "1"                      TO    SNDB-REC(1198:1).
     WRITE    SND-REC                  FROM  SNDB-REC.
     ADD      1                        TO    CNT-WRT-ALL
                                             CNT-WRT-B.
*
 MSG-OUT-B-EXIT.
     EXIT.
******************************************************************
*  　          発注元別出荷梱包リスト（Ｃ）出力
******************************************************************
 MSG-OUT-C-SEC     SECTION.
*
     MOVE     "MSG-OUT-C-SEC"          TO    S-NAME.
*
     MOVE     SPACE                    TO    SNDC-REC.
     INITIALIZE                              SNDC-REC.
*
*  制御バイト
     MOVE     X"28"                    TO    SNDC-S3041
                                             SNDC-S3081.
     MOVE     X"29"                    TO    SNDC-S3042
                                             SNDC-S3082.
*
     IF       CTL-FLG    =    "KON"
              GO                       TO    MSG-OUT-C-01
     ELSE
              GO                       TO    MSG-OUT-C-02
     END-IF.
*
*紐付情報データ基点
 MSG-OUT-C-01.
*  レコード区分
     MOVE     "C"                      TO    SNDC-F301.
*  発注者コード
     MOVE     SYK-F210                 TO    SNDC-F302.
*  発注者GLN
     MOVE     SYK-F211                 TO    SNDC-F303.
*  発注者名称
     MOVE     SYK-F212                 TO    SNDC-F304.
*  発注者名称カナ
     MOVE     SYK-F213                 TO    SNDC-F305.
*  最終納品先コード
     MOVE     SYK-F309                 TO    SNDC-F306.
*  最終納品先GLN
     MOVE     SYK-F310                 TO    SNDC-F307.
*  最終納品先名称
     MOVE     SYK-F311                 TO    SNDC-F308.
*  最終納品先名称カナ
     MOVE     SYK-F312                 TO    SNDC-F309.
*  カゴテナNO
*    F210 INIT
*
     GO                                TO    MSG-OUT-C-03.

*------------------
*欠品情報データ基点
 MSG-OUT-C-02.
*  レコード区分
     MOVE     "C"                      TO    SNDC-F301.
*  発注者コード
     MOVE     KEP-F210                 TO    SNDC-F302.
*  発注者GLN
     MOVE     KEP-F211                 TO    SNDC-F303.
*  発注者名称
     MOVE     KEP-F212                 TO    SNDC-F304.
*  発注者名称カナ
     MOVE     KEP-F213                 TO    SNDC-F305.
*  最終納品先コード
     MOVE     KEP-F309                 TO    SNDC-F306.
*  最終納品先GLN
     MOVE     KEP-F310                 TO    SNDC-F307.
*  最終納品先名称
     MOVE     KEP-F311                 TO    SNDC-F308.
*  最終納品先名称カナ
     MOVE     KEP-F312                 TO    SNDC-F309.
*  カゴテナNO
*    F210 INIT
*
     GO                                TO    MSG-OUT-C-03.
*------------------
 MSG-OUT-C-03.
*出荷梱包ＭＳＧ出力
     MOVE     "1"                      TO    SNDC-REC(1198:1).
     WRITE    SND-REC                  FROM  SNDC-REC.
     ADD      1                        TO    CNT-WRT-ALL
                                             CNT-WRT-C.
*
 MSG-OUT-C-EXIT.
     EXIT.
******************************************************************
*  　          出荷梱包内容（Ｄ）出力
******************************************************************
 MSG-OUT-D-SEC     SECTION.
*
     MOVE     "MSG-OUT-D-SEC"          TO    S-NAME.
*
     MOVE     SPACE                    TO    SNDD-REC.
     INITIALIZE                              SNDD-REC.
*
*  制御バイト
     MOVE     X"28"                    TO    SNDD-S4051
                                             SNDD-S4091.
     MOVE     X"29"                    TO    SNDD-S4052
                                             SNDD-S4122.
*
*紐付情報データ基点のみ
 MSG-OUT-D-01.
*  レコード区分
     MOVE     "D"                      TO    SNDD-F401.
*  親梱包NO
*    F402 INIT
*  梱包NO
     MOVE     KON-F09                  TO    SNDD-F403.
*  陳列場所コード
     MOVE     SYK-F317                 TO    SNDD-F404.
*  陳列場所名称
     MOVE     SYK-F318                 TO    SNDD-F405.
*  陳列場所名称カナ
     MOVE     SYK-F319                 TO    SNDD-F406.
*  販促開始日
     MOVE     SYK-F352                 TO    SNDD-F407.
     IF       SYK-F352                 =     ZERO
              MOVE     SPACE           TO    SNDD-REC(149:8)
     END-IF.
*  バーコード情報
     MOVE     SYK-F337                 TO    SNDD-F408.
*  カテゴリー名称1（印字用）
*    F409 INIT
*  カテゴリー名称2（印字用）
*    F410 INIT
*  最終納品先略称（印字用）
*    F411 INIT
*  ラベル自由使用欄（印字用）
*    F412 INIT
*  ラベル自由使用欄半角カナ（印字用）
*    F413 INIT
*------------------
 MSG-OUT-D-02.
*出荷梱包ＭＳＧ出力
     MOVE     "1"                      TO    SNDD-REC(1198:1).
     WRITE    SND-REC                  FROM  SNDD-REC.
     ADD      1                        TO    CNT-WRT-ALL
                                             CNT-WRT-D.
*
 MSG-OUT-D-EXIT.
     EXIT.
******************************************************************
*  　          取引明細（Ｅ）出力
******************************************************************
 MSG-OUT-E-SEC     SECTION.
*
     MOVE     "MSG-OUT-E-SEC"          TO    S-NAME.
*
     MOVE     SPACE                    TO    SNDE-REC.
     INITIALIZE                              SNDE-REC.
*
*紐付情報データ基点のみ
 MSG-OUT-E-01.
*  レコード区分
     MOVE     "E"                      TO    SNDE-F501.
*  取引番号（発注・返品）
     MOVE     SYK-F302                 TO    SNDE-F502.
*  取引明細番号（発注・返品）
     MOVE     SYK-F402                 TO    SNDE-F503.
*  発注日
     MOVE     SYK-F347                 TO    SNDE-F504.
*  最終納品先納品日
     MOVE     SYK-F349                 TO    SNDE-F505.
*  商品コード（GTIN）
     MOVE     SYK-F414                 TO    SNDE-F506.
*  商品コード（発注用）
     MOVE     SYK-F415                 TO    SNDE-F507.
*  商品コード区分
     MOVE     SYK-F416                 TO    SNDE-F508.
*  商品コード（取引先）
     MOVE     SYK-F417                 TO    SNDE-F509.
*  商品分類（大）
     MOVE     SYK-F345                 TO    SNDE-F510.
*  商品分類（中）
     MOVE     SYK-F346                 TO    SNDE-F511.
*  商品分類（小）
     MOVE     SYK-F407                 TO    SNDE-F512.
*  商品分類（細）
     MOVE     SYK-F408                 TO    SNDE-F513.
*  賞味期限日
*  F514 INIT
     IF       SYK-F505                 =     ZERO
              MOVE     SPACE           TO    SNDE-REC(117:8)
     END-IF.
*  製造日
*  F515 INIT
     IF       SYK-F506                 =     ZERO
              MOVE     SPACE           TO    SNDE-REC(125:8)
     END-IF.
*  製造番号
*  F516 INIT
*  商品区分
     MOVE     SYK-F354                 TO    SNDE-F517.
*  発注区分
     MOVE     SYK-F355                 TO    SNDE-F518.
*  EOS区分
     MOVE     SYK-F357                 TO    SNDE-F519.
*  処理種別
     MOVE     SYK-F361                 TO    SNDE-F520.
*  法定管理義務商材区分
     MOVE     SYK-F447                 TO    SNDE-F521.
*  発注単位
     MOVE     SYK-F453                 TO    SNDE-F522.
*  発注単位コード
     MOVE     SYK-F454                 TO    SNDE-F523.
*  発注数量（バラ）
     MOVE     SYK-F456                 TO    SNDE-F524.
*  発注数量（発注単位数）
     MOVE     SYK-F457                 TO    SNDE-F525.
*  出荷数量（バラ）
     MOVE     KON-F16                  TO    SNDE-F526.
*  出荷数量（発注単位数）
*    F527 INIT
*  原単価
     MOVE     SYK-F449                 TO    SNDE-F528.
*  売単価
     MOVE     SYK-F451                 TO    SNDE-F529.
*------------------
 MSG-OUT-E-02.
*出荷梱包ＭＳＧ出力
     MOVE     "1"                      TO    SNDE-REC(1198:1).
     WRITE    SND-REC                  FROM  SNDE-REC.
     ADD      1                        TO    CNT-WRT-ALL
                                             CNT-WRT-E.
*
 MSG-OUT-E-EXIT.
     EXIT.
******************************************************************
*  　          欠品情報（Ｇ）出力
******************************************************************
 MSG-OUT-G-SEC     SECTION.
*
     MOVE     "MSG-OUT-G-SEC"          TO    S-NAME.
*
     MOVE     SPACE                    TO    SNDG-REC.
     INITIALIZE                              SNDG-REC.
*
*欠品情報データ基点のみ
 MSG-OUT-G-01.
*  レコード区分
     MOVE     "G"                      TO    SNDG-F701.
*  欠品数量(バラ数)
     MOVE     KEP-F460                 TO    SNDG-F702.
*  欠品数量(発注単位数)
*    F703 INIT
*  欠品区分
     MOVE     KEP-F462                 TO    SNDG-F704.
*  取引番号（発注・返品）
     MOVE     KEP-F302                 TO    SNDG-F705.
*  取引明細番号（発注・返品）
     MOVE     KEP-F402                 TO    SNDG-F706.
*  発注日
     MOVE     KEP-F347                 TO    SNDG-F707.
*  最終納品先納品日
     MOVE     KEP-F349                 TO    SNDG-F708.
*  商品コード（GTIN）
     MOVE     KEP-F414                 TO    SNDG-F709.
*  商品コード（発注用）
     MOVE     KEP-F415                 TO    SNDG-F710.
*  商品コード区分
     MOVE     KEP-F416                 TO    SNDG-F711.
*  商品コード（取引先）
     MOVE     KEP-F417                 TO    SNDG-F712.
*  商品分類（大）
     MOVE     KEP-F345                 TO    SNDG-F713.
*  商品分類（中）
     MOVE     KEP-F346                 TO    SNDG-F714.
*  商品分類（小）
     MOVE     KEP-F407                 TO    SNDG-F715.
*  商品分類（細）
     MOVE     KEP-F408                 TO    SNDG-F716.
*  商品区分
     MOVE     KEP-F354                 TO    SNDG-F717.
*  発注区分
     MOVE     KEP-F355                 TO    SNDG-F718.
*  EOS区分
     MOVE     KEP-F357                 TO    SNDG-F719.
*  処理種別
     MOVE     KEP-F361                 TO    SNDG-F720.
*  発注単位
     MOVE     KEP-F453                 TO    SNDG-F721.
*  発注単位コード
     MOVE     KEP-F454                 TO    SNDG-F722.
*  発注数量（バラ）
     MOVE     KEP-F456                 TO    SNDG-F723.
*  発注数量（発注単位数）
*    F724 INIT
*  原単価
     MOVE     KEP-F449                 TO    SNDG-F725.
*  売単価
     MOVE     KEP-F451                 TO    SNDG-F726.
*------------------
 MSG-OUT-G-02.
*出荷梱包ＭＳＧ出力
     MOVE     "1"                      TO    SNDG-REC(1198:1).
     WRITE    SND-REC                  FROM  SNDG-REC.
     ADD      1                        TO    CNT-WRT-ALL
                                             CNT-WRT-G.
*
 MSG-OUT-G-EXIT.
     EXIT.
