# NKE1140B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE1140B.COB`

## ソースコード

```cobol
******************************************************************
*
* 　  顧客名             ： (株)サカタのタネ殿
*   　業務名　　　       ： ＨＧ基幹システム
*   　サブシステム名     ： 流通ＢＭＳ（カインズ店直)
*   　モジュール名       ： 出荷梱包送信データ作成
*   　作成日／作成者     ： 2019/07/19 NAV
*   　処理概要           ： 店直用各種データより、
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
 PROGRAM-ID.            NKE1140B.
*     流用元　　        NKE1130B(TC1)
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/07/19.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*カインズ出荷データ（店直)
     SELECT   CZSYKTN6  ASSIGN         DA-01-VI-CZSYKTN6
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SYK-F011
                                       SYK-F012
                                       SYK-F013
                                       SYK-F02
                                       SYK-F349
                                       SYK-F348
                                       SYK-F305
                                       SYK-F309
                                       SYK-F604
                                       SYK-F605
                                       SYK-F302
                                       SYK-F402
                        FILE STATUS    IS   SYK-ST.
*カインズ欠品データ（店直)
     SELECT   CZKEPTN1  ASSIGN         DA-01-VI-CZKEPTN1
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
*梱包数集計ファイル（店直)
     SELECT   CZKONGK1  ASSIGN         DA-01-VI-CZKONGK1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  NGK-F01
                                       NGK-F02
                                       NGK-F03
                                       NGK-F04
                                       NGK-F05
                                       NGK-F06
                                       NGK-F07
                        FILE STATUS    IS   NGK-ST.
*出荷梱包送信ワーク
     SELECT   WKONCZXX  ASSIGN    TO   DA-01-S-WKONCZXX
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   SND-ST.
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*カインズ出荷データ
******************************************************************
 FD  CZSYKTN6
                        LABEL     RECORD   IS   STANDARD.
     COPY     CZSYKTN6  OF        XFDLIB
              JOINING   SYK       PREFIX.
******************************************************************
*カインズ欠品データ（店直)
******************************************************************
 FD  CZKEPTN1
                        LABEL     RECORD   IS   STANDARD.
     COPY     CZKEPTN1  OF        XFDLIB
              JOINING   KEP       PREFIX.
******************************************************************
*流通ＢＭＳ発注メッセージ
******************************************************************
 FD  BMSHACL3
                        LABEL     RECORD   IS   STANDARD.
     COPY     BMSHACL3  OF        XFDLIB
              JOINING   HAC       PREFIX.
******************************************************************
*梱包数集計ファイル（店直)
******************************************************************
 FD  CZKONGK1
                        LABEL     RECORD   IS   STANDARD.
     COPY     CZKONGK1  OF        XFDLIB
              JOINING   NGK       PREFIX.
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
 01  NGK-INV                      PIC  X(03)     VALUE  SPACE.
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
*  出荷データ保管ＫＥＹ
 01  KEY-SYK.
     03  KEY-SYK-F.
         05  KEY-BDATE-SYK-F011   PIC  9(08)     VALUE  ZERO.
         05  KEY-BTIME-SYK-F012   PIC  9(04)     VALUE  ZERO.
         05  KEY-BTORI-SYK-F013   PIC  9(08)     VALUE  ZERO.
         05  KEY-SOKCD-SYK-F02    PIC  X(02)     VALUE  SPACE.
         05  KEY-NDATE-SYK-F349   PIC  9(08)     VALUE  ZERO.
         05  KEY-CDATE-SYK-F348   PIC  9(08)     VALUE  ZERO.
         05  KEY-CCODE-SYK-F305   PIC  9(05)     VALUE  ZERO.
         05  KEY-NCODE-SYK-F309   PIC  9(05)     VALUE  ZERO.
     03  KEY-OYANO-SYK-F604       PIC  X(22)     VALUE  SPACE.
     03  KEY-KONNO-SYK-F605       PIC  X(24)     VALUE  SPACE.
     03  KEY-DENNO-SYK-F302       PIC  9(09)     VALUE  ZERO.
     03  KEY-GYONO-SYK-F402       PIC  9(02)     VALUE  ZERO.
 01  NEW-SYK.
     03  NEW-SYK-F.
         05  NEW-BDATE-SYK-F011   PIC  9(08)     VALUE  ZERO.
         05  NEW-BTIME-SYK-F012   PIC  9(04)     VALUE  ZERO.
         05  NEW-BTORI-SYK-F013   PIC  9(08)     VALUE  ZERO.
         05  NEW-SOKCD-SYK-F02    PIC  X(02)     VALUE  SPACE.
         05  NEW-NDATE-SYK-F349   PIC  9(08)     VALUE  ZERO.
         05  NEW-CDATE-SYK-F348   PIC  9(08)     VALUE  ZERO.
         05  NEW-CCODE-SYK-F305   PIC  9(05)     VALUE  ZERO.
         05  NEW-NCODE-SYK-F309   PIC  9(05)     VALUE  ZERO.
     03  NEW-OYANO-SYK-F604       PIC  X(22)     VALUE  SPACE.
     03  NEW-KONNO-SYK-F605       PIC  X(24)     VALUE  SPACE.
     03  NEW-DENNO-SYK-F302       PIC  9(09)     VALUE  ZERO.
     03  NEW-GYONO-SYK-F402       PIC  9(02)     VALUE  ZERO.
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
     03  KEY-OYANO-KEP-FWWW       PIC  X(22)     VALUE  SPACE.
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
     03  NEW-OYANO-KEP-FWWW       PIC  X(22)     VALUE  SPACE.
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
     03  KEY-OYANO-SND            PIC  X(22)     VALUE  SPACE.
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
     COPY     CZSYKTN6  OF        XFDLIB
              JOINING   WSYK      PREFIX.
*ワーク明細データ退避
     COPY     SYKSNDF   OF        XFDLIB
              JOINING   WSND      PREFIX.
*
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  NGK-ST                   PIC  X(02).
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
     03  NGK-ERR                  PIC  N(20)     VALUE
         NC"梱包数集計ファイルエラ－".
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
         05  ST-PG                PIC  X(08)     VALUE "NKE1140B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NKE1140B".
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
 SYK-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CZSYKTN6.
     DISPLAY       SYK-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       SYK-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 KEP-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CZKEPTN1.
     DISPLAY       KEP-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       KEP-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 HAC-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  BMSHACL3.
     DISPLAY       HAC-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       HAC-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 NGK-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CZKONGK1.
     DISPLAY       NGK-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       NGK-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 SND-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  WKONCZXX.
     DISPLAY       SND-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       SND-ST   UPON      CONS.
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
     OPEN     I-O       CZSYKTN6
                        CZKEPTN1
                        BMSHACL3.
     OPEN     INPUT     CZKONGK1.
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
*出荷データＳＴＡＲＴ・ＲＥＡＤ
     MOVE     SPACE                TO  END-FLG1.
     PERFORM  CZSYKTN6-START-SEC.
*
     IF       END-FLG1    =       "   "
              PERFORM  CZSYKTN6-READ-SEC
     END-IF.
*
     IF       END-FLG1    =       "END"
              DISPLAY NC"＃　出荷データなし　＃" UPON CONS
              MOVE    HIGH-VALUE   TO  KEY-SYK
     END-IF.
*    出荷データＫＥＹ保管
     IF       END-FLG1    =       "HIT"
              MOVE     SYK-F011        TO       KEY-BDATE-SYK-F011
                                                NEW-BDATE-SYK-F011
              MOVE     SYK-F012        TO       KEY-BTIME-SYK-F012
                                                NEW-BTIME-SYK-F012
              MOVE     SYK-F013        TO       KEY-BTORI-SYK-F013
                                                NEW-BTORI-SYK-F013
              MOVE     SYK-F02         TO       KEY-SOKCD-SYK-F02
                                                NEW-SOKCD-SYK-F02
              MOVE     SYK-F349        TO       KEY-NDATE-SYK-F349
                                                NEW-NDATE-SYK-F349
              MOVE     SYK-F348        TO       KEY-CDATE-SYK-F348
                                                NEW-CDATE-SYK-F348
              MOVE     SYK-F305(1:5)   TO       KEY-CCODE-SYK-F305
                                                NEW-CCODE-SYK-F305
              MOVE     SYK-F309(1:5)   TO       KEY-NCODE-SYK-F309
                                                NEW-NCODE-SYK-F309
              MOVE     SYK-F604        TO       KEY-OYANO-SYK-F604
                                                NEW-OYANO-SYK-F604
              MOVE     SYK-F605        TO       KEY-KONNO-SYK-F605
                                                NEW-KONNO-SYK-F605
              MOVE     SYK-F302        TO       KEY-DENNO-SYK-F302
                                                NEW-DENNO-SYK-F302
              MOVE     SYK-F402        TO       KEY-GYONO-SYK-F402
                                                NEW-GYONO-SYK-F402
     END-IF.
*
*欠品データＳＴＡＲＴ・ＲＥＡＤ
     MOVE     SPACE                TO  END-FLG2.
     PERFORM  CZKEPTN1-START-SEC.
*
     IF       END-FLG2    =       "   "
              PERFORM  CZKEPTN1-READ-SEC
     END-IF.
*
     IF       END-FLG2    =       "END"
              DISPLAY NC"＃　欠品データなし　＃" UPON CONS
              MOVE    HIGH-VALUE   TO  KEY-KEP
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
              MOVE     ALL "9"         TO     KEY-OYANO-KEP-FWWW
                                              NEW-OYANO-KEP-FWWW
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
              MOVE     "SYK"     TO    CTL-FLG
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
*    DISPLAY "KEY-SYK-F=" KEY-SYK-F.
*    DISPLAY "KEY-KEP-F=" KEY-KEP-F.
*T↑
*
*ＫＥＹ比較
*        紐付ＫＥＹ <　欠品ＫＥＹ
     IF  KEY-SYK-F  <  KEY-KEP-F
*T↓
*    DISPLAY "KEY-SYK-F < KEY-SYK-F"
*T↑
*
*        発注ＭＳＧ検索
         MOVE         SYK-F011   TO    HAC-F011
         MOVE         SYK-F012   TO    HAC-F012
         MOVE         SYK-F013   TO    HAC-F013
         MOVE         SYK-F02    TO    HAC-F02
         MOVE         SYK-F349   TO    HAC-F346
         MOVE         SYK-F309   TO    HAC-F308
         MOVE         SYK-F302   TO    HAC-F302
         MOVE         SYK-F402   TO    HAC-F402
         PERFORM      BMSHACL3-READ-SEC
         IF           HAC-INV  = "INV"
                      DISPLAY NC"発注データが存在しません！"
                                                      UPON CONS
                      MOVE       "4010"  TO   PROGRAM-STATUS
                      STOP     RUN
         END-IF
*
         IF  KEY-SYK-F  <  KEY-SND-F
             DISPLAY NC"＃　データ不整合！１＃" UPON CONS
*T↓
*        DISPLAY "KEY-SYK-F=" KEY-SYK-F UPON CONS
*        DISPLAY "KEY-SND-F=" KEY-SND-F UPON CONS
*T↑
             MOVE    "4010"        TO        PROGRAM-STATUS
             STOP    RUN
         END-IF
*
         IF  KEY-SYK-F  =  KEY-SND-F
*
             IF  ( KEY-CDATE-SYK-F348  NOT = KEY-CDATE-SND ) OR
                 ( KEY-CCODE-SYK-F305  NOT = KEY-CCODE-SND )
*                  出荷梱包リスト（Ｂ）出力
                   MOVE     "SYK"     TO    CTL-FLG
                   PERFORM  MSG-OUT-B-SEC
             END-IF
*
             IF  ( KEY-NCODE-SYK-F309  NOT = KEY-NCODE-SND )
*                  発注元別出荷梱包リスト（Ｃ）出力
                   MOVE     "SYK"     TO    CTL-FLG
                   PERFORM  MSG-OUT-C-SEC
             END-IF
*
             IF  ( KEY-OYANO-SYK-F604  NOT = KEY-OYANO-SND ) OR
                 ( KEY-KONNO-SYK-F605  NOT = KEY-KONNO-SND )
*                  出荷梱包内容（Ｄ）出力
                   PERFORM  MSG-OUT-D-SEC
             END-IF
*
*            取引明細（Ｅ）出力
             PERFORM  MSG-OUT-E-SEC
         END-IF
*
         IF  KEY-SYK-F  >  KEY-SND-F
*
             IF  ( KEY-CDATE-SYK-F348  NOT = KEY-CDATE-SND ) OR
                 ( KEY-CCODE-SYK-F305  NOT = KEY-CCODE-SND )
*                  出荷梱包リスト（Ｂ）出力
                   MOVE     "SYK"     TO    CTL-FLG
                   PERFORM  MSG-OUT-B-SEC
             END-IF
*
             IF  ( KEY-NCODE-SYK-F309  NOT = KEY-NCODE-SND )
*                  発注元別出荷梱包リスト（Ｃ）出力
                   MOVE     "SYK"     TO    CTL-FLG
                   PERFORM  MSG-OUT-C-SEC
             END-IF
*
             IF  ( KEY-OYANO-SYK-F604  NOT = KEY-OYANO-SND ) OR
                 ( KEY-KONNO-SYK-F605  NOT = KEY-KONNO-SND )
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
*        データ作成ＫＥＹ保管
         MOVE     SYK-F011        TO       KEY-BDATE-SND
         MOVE     SYK-F012        TO       KEY-BTIME-SND
         MOVE     SYK-F013        TO       KEY-BTORI-SND
         MOVE     SYK-F02         TO       KEY-SOKCD-SND
         MOVE     SYK-F349        TO       KEY-NDATE-SND
         MOVE     SYK-F348        TO       KEY-CDATE-SND
         MOVE     SYK-F305(1:5)   TO       KEY-CCODE-SND
         MOVE     SYK-F309(1:5)   TO       KEY-NCODE-SND
         MOVE     SYK-F604        TO       KEY-OYANO-SND
         MOVE     SYK-F605        TO       KEY-KONNO-SND
         MOVE     SYK-F302        TO       KEY-DENNO-SND
         MOVE     SYK-F402        TO       KEY-GYONO-SND
*
*        カインズ出荷データ更新
         PERFORM  CZSYKTN6-UPDT-SEC
*
*        カインズ出荷データ次ＲＥＡＤ
         PERFORM  CZSYKTN6-READ-SEC
*
*        紐付情報ＫＥＹ保管
         IF       END-FLG1    =       "HIT"
                  MOVE     SYK-F011      TO   KEY-BDATE-SYK-F011
                                              NEW-BDATE-SYK-F011
                  MOVE     SYK-F012      TO   KEY-BTIME-SYK-F012
                                              NEW-BTIME-SYK-F012
                  MOVE     SYK-F013      TO   KEY-BTORI-SYK-F013
                                              NEW-BTORI-SYK-F013
                  MOVE     SYK-F02       TO   KEY-SOKCD-SYK-F02
                                              NEW-SOKCD-SYK-F02
                  MOVE     SYK-F349      TO   KEY-NDATE-SYK-F349
                                              NEW-NDATE-SYK-F349
                  MOVE     SYK-F348      TO   KEY-CDATE-SYK-F348
                                              NEW-CDATE-SYK-F348
                  MOVE     SYK-F305(1:5) TO   KEY-CCODE-SYK-F305
                                              NEW-CCODE-SYK-F305
                  MOVE     SYK-F309(1:5) TO   KEY-NCODE-SYK-F309
                                              NEW-NCODE-SYK-F309
                  MOVE     SYK-F604      TO   KEY-OYANO-SYK-F604
                                              NEW-OYANO-SYK-F604
                  MOVE     SYK-F605      TO   KEY-KONNO-SYK-F605
                                              NEW-KONNO-SYK-F605
                  MOVE     SYK-F302      TO   KEY-DENNO-SYK-F302
                                              NEW-DENNO-SYK-F302
                  MOVE     SYK-F402      TO   KEY-GYONO-SYK-F402
                                              NEW-GYONO-SYK-F402
         END-IF
*
         GO       TO       MAIN-EXIT
*
     END-IF.
*----------------------------------------------------------------
 MAIN-02.
*ＫＥＹ比較
*        紐付ＫＥＹ >　欠品ＫＥＹ
     IF  KEY-SYK-F  >  KEY-KEP-F
*T↓
*    DISPLAY "KEY-SYK-F > KEY-SYK-F"
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
*
*****    DISPLAY "KEY-KEP-F = " KEY-KEP-F UPON CONS
*****    DISPLAY "KEY-SND-F = " KEY-SND-F UPON CONS
         IF  KEY-KEP-F  <  KEY-SND-F
             DISPLAY NC"＃　データ不整合！２＃" UPON CONS
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
*        発注ＭＳＧ更新
         PERFORM  BMSHACL3-UPDT-SEC
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
         MOVE     ALL "9"         TO       KEY-OYANO-SND
         MOVE     ALL "9"         TO       KEY-KONNO-SND
         MOVE     KEP-F302        TO       KEY-DENNO-SND
         MOVE     KEP-F402        TO       KEY-GYONO-SND
*
*        カインズ欠品データ更新
         PERFORM  CZKEPTN1-UPDT-SEC
*
*        カインズ欠品データＲＥＡＤ
         PERFORM  CZKEPTN1-READ-SEC
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
                  MOVE     ALL "9"         TO   KEY-OYANO-KEP-FWWW
                                                NEW-OYANO-KEP-FWWW
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
     IF  KEY-SYK-F  =  KEY-KEP-F
*T↓
*    DISPLAY "KEY-SYK-F = KEY-SYK-F"
*T↑
*
         PERFORM UNTIL
                 (( SYK-F011      NOT =  KEY-BDATE-SYK-F011 )  OR
                  ( SYK-F012      NOT =  KEY-BTIME-SYK-F012 )  OR
                  ( SYK-F013      NOT =  KEY-BTORI-SYK-F013 )  OR
                  ( SYK-F02       NOT =  KEY-SOKCD-SYK-F02  )  OR
                  ( SYK-F349      NOT =  KEY-NDATE-SYK-F349 )  OR
                  ( SYK-F348      NOT =  KEY-CDATE-SYK-F348 )  OR
                  ( SYK-F305(1:5) NOT =  KEY-CCODE-SYK-F305 )  OR
                  ( SYK-F309(1:5) NOT =  KEY-NCODE-SYK-F309 )) OR
                  ( END-FLG1          =  "END"              )
*
*            発注ＭＳＧ検索
             MOVE         SYK-F011   TO    HAC-F011
             MOVE         SYK-F012   TO    HAC-F012
             MOVE         SYK-F013   TO    HAC-F013
             MOVE         SYK-F02    TO    HAC-F02
             MOVE         SYK-F349   TO    HAC-F346
             MOVE         SYK-F309   TO    HAC-F308
             MOVE         SYK-F302   TO    HAC-F302
             MOVE         SYK-F402   TO    HAC-F402
             PERFORM      BMSHACL3-READ-SEC
             IF           HAC-INV  = "INV"
                          DISPLAY NC"発注データが存在しません！"
                                                      UPON CONS
                          MOVE       "4010"  TO   PROGRAM-STATUS
                          STOP     RUN
             END-IF
*
             IF  NEW-SYK-F  <  KEY-SND-F
                 DISPLAY NC"＃　データ不整合！３＃" UPON CONS
                 MOVE    "4010"        TO        PROGRAM-STATUS
                 STOP    RUN
             END-IF
*
             IF  NEW-SYK-F  =  KEY-SND-F
*
                 IF  ( NEW-CDATE-SYK-F348  NOT = KEY-CDATE-SND )
                    OR
                     ( NEW-CCODE-SYK-F305  NOT = KEY-CCODE-SND )
*                      出荷梱包リスト（Ｂ）出力
                       MOVE     "SYK"     TO    CTL-FLG
                       PERFORM  MSG-OUT-B-SEC
                 END-IF
*
                 IF  ( NEW-NCODE-SYK-F309  NOT = KEY-NCODE-SND )
*                      発注元別出荷梱包リスト（Ｃ）出力
                       MOVE     "SYK"     TO    CTL-FLG
                       PERFORM  MSG-OUT-C-SEC
                 END-IF
*
                 IF  ( NEW-OYANO-SYK-F604  NOT = KEY-OYANO-SND )
                  OR ( NEW-KONNO-SYK-F605  NOT = KEY-KONNO-SND )
*                      出荷梱包内容（Ｄ）出力
                       PERFORM  MSG-OUT-D-SEC
                 END-IF
*
*                取引明細（Ｅ）出力
                 PERFORM  MSG-OUT-E-SEC
             END-IF
*
             IF  NEW-SYK-F  >  KEY-SND-F
*
                 IF  ( NEW-CDATE-SYK-F348  NOT = KEY-CDATE-SND )
                    OR
                     ( NEW-CCODE-SYK-F305  NOT = KEY-CCODE-SND )
*                      出荷梱包リスト（Ｂ）出力
                       MOVE     "SYK"     TO    CTL-FLG
                       PERFORM  MSG-OUT-B-SEC
                 END-IF
*
                 IF  ( NEW-NCODE-SYK-F309  NOT = KEY-NCODE-SND )
*                      発注元別出荷梱包リスト（Ｃ）出力
                       MOVE     "SYK"     TO    CTL-FLG
                       PERFORM  MSG-OUT-C-SEC
                 END-IF
*
                 IF  ( NEW-OYANO-SYK-F604  NOT = KEY-OYANO-SND )
                  OR ( NEW-KONNO-SYK-F605  NOT = KEY-KONNO-SND )
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
*            データ作成ＫＥＹ保管
             MOVE     SYK-F011        TO       KEY-BDATE-SND
             MOVE     SYK-F012        TO       KEY-BTIME-SND
             MOVE     SYK-F013        TO       KEY-BTORI-SND
             MOVE     SYK-F02         TO       KEY-SOKCD-SND
             MOVE     SYK-F349        TO       KEY-NDATE-SND
             MOVE     SYK-F348        TO       KEY-CDATE-SND
             MOVE     SYK-F305(1:5)   TO       KEY-CCODE-SND
             MOVE     SYK-F309(1:5)   TO       KEY-NCODE-SND
             MOVE     SYK-F604        TO       KEY-OYANO-SND
             MOVE     SYK-F605        TO       KEY-KONNO-SND
             MOVE     SYK-F302        TO       KEY-DENNO-SND
             MOVE     SYK-F402        TO       KEY-GYONO-SND
*
*            カインズ出荷データ処理制御部更新
             PERFORM      CZSYKTN6-UPDT-SEC
*
*            カインズ出荷データ次ＲＥＡＤ
             PERFORM  CZSYKTN6-READ-SEC
*
*            最新ＫＥＹ保管
             MOVE     SYK-F011        TO       NEW-BDATE-SYK-F011
             MOVE     SYK-F012        TO       NEW-BTIME-SYK-F012
             MOVE     SYK-F013        TO       NEW-BTORI-SYK-F013
             MOVE     SYK-F02         TO       NEW-SOKCD-SYK-F02
             MOVE     SYK-F349        TO       NEW-NDATE-SYK-F349
             MOVE     SYK-F348        TO       NEW-CDATE-SYK-F348
             MOVE     SYK-F305(1:5)   TO       NEW-CCODE-SYK-F305
             MOVE     SYK-F309(1:5)   TO       NEW-NCODE-SYK-F309
             MOVE     SYK-F604        TO       NEW-OYANO-SYK-F604
             MOVE     SYK-F605        TO       NEW-KONNO-SYK-F605
             MOVE     SYK-F302        TO       NEW-DENNO-SYK-F302
             MOVE     SYK-F402        TO       NEW-GYONO-SYK-F402
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
*
*            欠品情報（Ｇ）出力
             PERFORM  MSG-OUT-G-SEC
*
*            発注ＭＳＧ更新
             PERFORM      BMSHACL3-UPDT-SEC
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
             MOVE     ALL "9"         TO       KEY-OYANO-SND
             MOVE     ALL "9"         TO       KEY-KONNO-SND
             MOVE     KEP-F302        TO       KEY-DENNO-SND
             MOVE     KEP-F402        TO       KEY-GYONO-SND
*
*            カインズ欠品データ処理制御部更新
             PERFORM  CZKEPTN1-UPDT-SEC
*
*            カインズ欠品データ次ＲＥＡＤ
             PERFORM  CZKEPTN1-READ-SEC
*
*            最新ＫＥＹ保管
             MOVE     KEP-F011        TO       NEW-BDATE-KEP-F011
             MOVE     KEP-F012        TO       NEW-BTIME-KEP-F012
             MOVE     KEP-F013        TO       NEW-BTORI-KEP-F013
             MOVE     KEP-F02         TO       NEW-SOKCD-KEP-F02
             MOVE     KEP-F349        TO       NEW-NDATE-KEP-F349
             MOVE     KEP-F348        TO       NEW-CDATE-KEP-F348
             MOVE     KEP-F305(1:5)   TO       NEW-CCODE-KEP-F305
             MOVE     KEP-F309(1:5)   TO       NEW-NCODE-KEP-F309
             MOVE     ALL "9"         TO       NEW-OYANO-KEP-FWWW
             MOVE     ALL "9"         TO       NEW-KONNO-KEP-FXXX
             MOVE     KEP-F302        TO       NEW-DENNO-KEP-F302
             MOVE     KEP-F402        TO       NEW-GYONO-KEP-F402
*
         END-PERFORM
*        -------------------------------------------------------
*        出荷データＫＥＹ保管
         IF       END-FLG1    =    "HIT"
                  MOVE     SYK-F011      TO   KEY-BDATE-SYK-F011
                  MOVE     SYK-F012      TO   KEY-BTIME-SYK-F012
                  MOVE     SYK-F013      TO   KEY-BTORI-SYK-F013
                  MOVE     SYK-F02       TO   KEY-SOKCD-SYK-F02
                  MOVE     SYK-F349      TO   KEY-NDATE-SYK-F349
                  MOVE     SYK-F348      TO   KEY-CDATE-SYK-F348
                  MOVE     SYK-F305(1:5) TO   KEY-CCODE-SYK-F305
                  MOVE     SYK-F309(1:5) TO   KEY-NCODE-SYK-F309
                  MOVE     SYK-F604      TO   KEY-OYANO-SYK-F604
                  MOVE     SYK-F605      TO   KEY-KONNO-SYK-F605
                  MOVE     SYK-F302      TO   KEY-DENNO-SYK-F302
                  MOVE     SYK-F402      TO   KEY-GYONO-SYK-F402
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
                  MOVE     ALL "9"       TO   KEY-OYANO-KEP-FWWW
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
     CLOSE     CZSYKTN6 CZKEPTN1 BMSHACL3 CZKONGK1 WKONCZXX.
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
*            出荷データ（カインズ店直）ＳＴＡＲＴ
******************************************************************
 CZSYKTN6-START-SEC          SECTION.
*
     MOVE    "CZSYKTN6-START-SEC"         TO   S-NAME.
*
 CZSYKTN6-START-100.
*
     MOVE     SPACE                       TO   SYK-REC.
     INITIALIZE                                SYK-REC.
     MOVE     PARA-IN-BDATE               TO   SYK-F011.
     MOVE     PARA-IN-BTIME               TO   SYK-F012.
     MOVE     PARA-IN-BTORI               TO   SYK-F013.
     MOVE     PARA-IN-SOKCD               TO   SYK-F02.
     MOVE     PARA-IN-NDATE               TO   SYK-F349.
*
     START    CZSYKTN6       KEY    IS     >=  SYK-F011
                                               SYK-F012
                                               SYK-F013
                                               SYK-F02
                                               SYK-F349
                                               SYK-F348
                                               SYK-F305
                                               SYK-F309
                                               SYK-F604
                                               SYK-F605
                                               SYK-F302
                                               SYK-F402
              INVALID
                     MOVE  "END"   TO          END-FLG1
                     GO            TO          CZSYKTN6-START-EXIT
     END-START.
*
 CZSYKTN6-START-EXIT.
     EXIT.
*
******************************************************************
*            出荷データ（カインズ店直）読込
******************************************************************
 CZSYKTN6-READ-SEC           SECTION.
*
     MOVE    "CZSYKTN6-READ-SEC"          TO   S-NAME.
*
 CZSYKTN6-READ-100.
*
     READ     CZSYKTN6
              AT  END       MOVE  "END"        TO  END-FLG1
                            MOVE   HIGH-VALUE  TO  KEY-SYK
                            GO     TO     CZSYKTN6-READ-EXIT
              NOT AT  END   ADD    1      TO   RD-CNT1
     END-READ.
*
*件数表示
     IF       RD-CNT1(6:3)   =  "000"  OR  "500"
              DISPLAY "# READ-CNT = " RD-CNT1
              UPON CONS
     END-IF.
* 範囲判定
     IF   ( SYK-F011  =  PARA-IN-BDATE ) AND
          ( SYK-F012  =  PARA-IN-BTIME ) AND
          ( SYK-F013  =  PARA-IN-BTORI ) AND
          ( SYK-F02   =  PARA-IN-SOKCD ) AND
          ( SYK-F349  =  PARA-IN-NDATE )
          CONTINUE
     ELSE
          MOVE   "END"   TO   END-FLG1
          GO             TO   CZSYKTN6-READ-EXIT
     END-IF.
*
*    出荷送信済フラグ判定
     IF   SYK-F601  =  " "
          CONTINUE
     ELSE
          GO  TO    CZSYKTN6-READ-100
     END-IF.
*
*    出荷数判定
     IF   SYK-F458  NOT =  ZERO
          CONTINUE
     ELSE
*         処理制御部更新し読み飛ばし
          PERFORM   CZSYKTN6-UPDT-SEC
          GO  TO    CZSYKTN6-READ-100
     END-IF.
*
* 発注ＭＳＧ存在チェック
     MOVE     SYK-F011   TO    HAC-F011.
     MOVE     SYK-F012   TO    HAC-F012.
     MOVE     SYK-F013   TO    HAC-F013.
     MOVE     SYK-F02    TO    HAC-F02.
     MOVE     SYK-F349   TO    HAC-F346.
     MOVE     SYK-F309   TO    HAC-F308.
     MOVE     SYK-F302   TO    HAC-F302.
     MOVE     SYK-F402   TO    HAC-F402.
     PERFORM  BMSHACL3-READ-SEC.
     IF       HAC-INV  = "INV"
              DISPLAY NC"発注データが存在しません！" UPON CONS
              MOVE       "4010"  TO   PROGRAM-STATUS
              STOP       RUN
     ELSE
              MOVE      "HIT"    TO   END-FLG1
     END-IF.
*
 CZSYKTN6-READ-EXIT.
     EXIT.
*
******************************************************************
*            カインズ欠品データ(店直)ＳＴＡＲＴ
******************************************************************
 CZKEPTN1-START-SEC          SECTION.
*
     MOVE    "CZKEPTN1-START-SEC"         TO   S-NAME.
*
 CZKEPTN1-START-100.
*
     MOVE     SPACE                       TO   KEP-REC.
     INITIALIZE                                KEP-REC.
     MOVE     PARA-IN-BDATE               TO   KEP-F011.
     MOVE     PARA-IN-BTIME               TO   KEP-F012.
     MOVE     PARA-IN-BTORI               TO   KEP-F013.
     MOVE     PARA-IN-SOKCD               TO   KEP-F02.
     MOVE     PARA-IN-NDATE               TO   KEP-F349.
*
     START    CZKEPTN1      KEY    IS     >=   KEP-F011
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
                     GO            TO          CZKEPTN1-START-EXIT
     END-START.
*
 CZKEPTN1-START-EXIT.
     EXIT.
*
******************************************************************
*            カインズ欠品データ(店直)読込
******************************************************************
 CZKEPTN1-READ-SEC           SECTION.
*
     MOVE    "CZKEPTN1-READ-SEC"          TO   S-NAME.
*
 CZKEPTN1-READ-100.
*
     READ     CZKEPTN1
              AT  END       MOVE  "END"        TO  END-FLG2
                            MOVE   HIGH-VALUE  TO  KEY-KEP
                            GO     TO     CZKEPTN1-READ-EXIT
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
          GO             TO   CZKEPTN1-READ-EXIT
     END-IF.
*
 CZKEPTN1-READ-EXIT.
     EXIT.
*
******************************************************************
*            カインズ欠品データ更新
******************************************************************
 CZKEPTN1-UPDT-SEC          SECTION.
*
     MOVE    "CZKEPTN1-UPDT-SEC"          TO   S-NAME.
*
 CZKEPTN1-UPDT-100.
*
     MOVE    "1"                          TO   KEP-F601.
     MOVE     SYS-DATEW                   TO   KEP-F602.
     MOVE     SYS-HHMMSS                  TO   KEP-F603.
*
     REWRITE  KEP-REC.
*
 CZKEPTN1-UPDT-EXIT.
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
*            カインズ出荷データ更新
******************************************************************
 CZSYKTN6-UPDT-SEC          SECTION.
*
     MOVE    "CZSYKTN6-UPDT-SEC"          TO   S-NAME.
*
 CZSYKTN6-UPDT-100.
*
     MOVE    "1"                          TO   SYK-F601.
     MOVE     SYS-DATEW                   TO   SYK-F602.
     MOVE     SYS-HHMMSS                  TO   SYK-F603.
*
     REWRITE  SYK-REC.
*
 CZSYKTN6-UPDT-EXIT.
     EXIT.
*
******************************************************************
*            梱包数集計ファイル（店直)存在チェック
******************************************************************
 CZKONGK1-READ-SEC           SECTION.
*
     MOVE    "CZKONGK1-READ-SEC"          TO   S-NAME.
*
 CZKONGK1-READ-100.
*
     READ     CZKONGK1
              INVALID
                     MOVE     "INV"       TO   NGK-INV
              NOT INVALID
                     MOVE     "   "       TO   NGK-INV
     END-READ.
*
 CZKONGK1-READ-EXIT.
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
     IF       CTL-FLG    =    "SYK"
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
     IF       CTL-FLG    =    "SYK"
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
     MOVE     SYK-F011                 TO    NGK-F01.
     MOVE     SYK-F012                 TO    NGK-F02.
     MOVE     SYK-F013                 TO    NGK-F03.
     MOVE     SYK-F02                  TO    NGK-F04.
     MOVE     SYK-F349                 TO    NGK-F05.
     MOVE     SYK-F348                 TO    NGK-F06.
     MOVE     SYK-F305(1:5)            TO    NGK-F07.
     PERFORM  CZKONGK1-READ-SEC.
     IF       NGK-INV   =   "   "
*T↓
*    DISPLAY  "NGK-F01=" NGK-F01 UPON CONS
*    DISPLAY  "NGK-F02=" NGK-F02 UPON CONS
*    DISPLAY  "NGK-F03=" NGK-F03 UPON CONS
*    DISPLAY  "NGK-F04=" NGK-F04 UPON CONS
*    DISPLAY  "NGK-F05=" NGK-F05 UPON CONS
*    DISPLAY  "NGK-F06=" NGK-F06 UPON CONS
*    DISPLAY  "NGK-F07=" NGK-F07 UPON CONS
*    DISPLAY  "NGK-F08=" NGK-F08 UPON CONS
*    DISPLAY  "SNDB-F226=" SNDB-F226 UPON CONS
*T↑
              MOVE     NGK-F08(2:6)    TO    SNDB-F226
     ELSE
*             仕様上、発生しないケース
              DISPLAY NC"＃　データ不整合？？４＃"
                                             UPON CONS
              MOVE    "4010"           TO    PROGRAM-STATUS
              STOP     RUN
     END-IF.
*
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
     IF       KEP-F350                 =     ZERO
              MOVE     SPACE           TO    SNDB-REC(687:8)
     END-IF.
*
     GO                                TO    MSG-OUT-B-03.
*------------------
 MSG-OUT-B-03.
*出荷梱包ＭＳＧ出力
*T↓
*    DISPLAY  "SNDB-F226=" SNDB-F226 UPON CONS.
*T↑
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
     IF       CTL-FLG    =    "SYK"
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
*
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
*
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
     MOVE     SYK-F604                 TO    SNDD-F402.
*  梱包NO
     MOVE     SYK-F605                 TO    SNDD-F403.
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
     MOVE     SYK-F458                 TO    SNDE-F526.
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

```
