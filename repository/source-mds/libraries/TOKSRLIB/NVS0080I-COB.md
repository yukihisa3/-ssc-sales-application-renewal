# NVS0080I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0080I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　Ｄ３６５手動送受信指示　　　　　　*
*    作成日／作成者　　　：　2020/03/27   ASS.II          04
*    処理内容　　　　　　：　画面よりD365手動送受信を行う　　
*    　　　　　　　　　　　　送受信グループを入力し、　　　　　*
*    　　　　　　　　　　　　パラメタに必要情報を出力する。　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0080I.
 AUTHOR.               ASS.II.
 DATE-WRITTEN.         20/03/04.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
*Ｄ365定例ジョブ設定マスタ
     SELECT  DNJOBSF   ASSIGN    TO        DA-01-VI-DNJOBSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       JOB-F01
                                           JOB-F02
                       FILE      STATUS    JOB-ST.
*D365送受信グループマスタ
     SELECT  DNGROPF   ASSIGN    TO        DA-01-VI-DNGROPL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       GRP-F01
                       FILE      STATUS    GRP-ST.

*
*D365送受信データ種別マスタ
     SELECT   DNDATSL1 ASSIGN    TO        DA-01-VI-DNDATSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       DAT-F01
                       FILE  STATUS    IS  DAT-ST.
*条件ファイル
     SELECT   HJYOKEN        ASSIGN        TO  DA-01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FVS00801  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
*    FILE = Ｄ365定例ジョブ設定マスタ
****************************************************************
 FD  DNJOBSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      DNJOBSF   OF   XFDLIB
                       JOINING   JOB       AS   PREFIX.
****************************************************************
*    FILE = D365送受信グループマスタ
****************************************************************
 FD  DNGROPF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      DNGROPF   OF   XFDLIB
                       JOINING   GRP       AS   PREFIX.
*
******************************************************************
*    D365送受信データ種別マスタ
******************************************************************
 FD  DNDATSL1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      DNDATSF   OF   XFDLIB
                       JOINING   DAT       AS   PREFIX.
*
****************************************************************
*    FILE = 条件ファイル                                     *
****************************************************************
 FD  HJYOKEN
                        LABEL     RECORD    IS   STANDARD.
                        COPY      HJYOKEN   OF   XFDLIB
                        JOINING   JYO       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  JOB-ST                   PIC  X(02).
     03  GRP-ST                   PIC  X(02).
     03  DAT-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "MVS00801".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FVS00801".
*フラグ領域
 01  FLG-AREA.
     03  MAIN-FLG                 PIC  9(02)  VALUE  ZERO.
     03  PFK-FLG                  PIC  9(02)  VALUE  ZERO.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  GRP-INV-FLG              PIC  X(03)  VALUE  SPACE.
     03  DAT-INV-FLG              PIC  X(03)  VALUE  SPACE.
     03  JYO-INV-FLG              PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  モード退避
     03  WK-DTSYU                 PIC  X(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了　_項目戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"送受信グループマスタ未登録です。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"発行日付区分は、１，２，３です。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"送受信グループ_が未入力です。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"発行済区分エラー".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS   6  PIC   N(20).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  JOB-ERR           PIC N(20) VALUE
                        NC"定例ジョブ設定マスタエラー".
     03  GRP-ERR           PIC N(20) VALUE
                        NC"送受信グループマスタエラー".
     03  DAT-ERR           PIC N(20) VALUE
                        NC"送受信データ種別マスタエラー".
     03  JYO-ERR           PIC N(20) VALUE
                        NC"条件ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD8         PIC 9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-OUT-DATE         PIC 9(08).
 01  LINK-OUT-TIME         PIC 9(04).
 01  LINK-OUT-GROP         PIC 9(03).
 01  LINK-OUT-RUNNO        PIC 9(07).
*
**************************************************************
 PROCEDURE       DIVISION  USING
                                      LINK-OUT-DATE
                                      LINK-OUT-TIME
                                      LINK-OUT-GROP
                                      LINK-OUT-RUNNO.
**************************************************************
 DECLARATIVES.
 DSP-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 JOB-ERR-SEC               SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DNJOBSF.
     DISPLAY     JOB-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JOB-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 GRP-ERR-SEC               SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DNGROPF.
     DISPLAY     GRP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     GRP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DAT-ERR-SEC               SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DNDATSL1.
     DISPLAY     DAT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DAT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR-SEC               SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HJYOKEN.
     DISPLAY     JYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL     MAIN-FLG  =  99.
     PERFORM   END-SEC.
***
     STOP    RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     DNJOBSF.
     OPEN     INPUT     DNGROPF.
     OPEN     INPUT     DNDATSL1.
     OPEN     I-O       HJYOKEN.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
*ワークの初期化
     INITIALIZE         FLG-AREA
                        LINK-OUT-DATE
                        LINK-OUT-TIME
                        LINK-OUT-GROP
                        LINK-OUT-RUNNO.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ヘッド入力へ
     MOVE      1                  TO   MAIN-FLG.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      MAIN-FLG
*初期画面表示
         WHEN   1      PERFORM  DSP-INIT-SEC
*ＢＯＤＹ部入力
         WHEN   2      PERFORM  DSP-BODY-SEC
*確認入力
         WHEN   3      PERFORM  DSP-KAKU-SEC
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE.
     CLOSE             DNJOBSF.
     CLOSE             DNGROPF.
     CLOSE             DNDATSL1.
     CLOSE             HJYOKEN.

     DISPLAY "日付          ="  LINK-OUT-DATE   UPON CONS.
     DISPLAY "時刻          ="  LINK-OUT-TIME   UPON CONS.
     DISPLAY "送受信グループ="  LINK-OUT-GROP   UPON CONS.
     DISPLAY "実行NO        ="  LINK-OUT-RUNNO  UPON CONS.
     DISPLAY "PROGRAM-STATUS="  PROGRAM-STATUS  UPON CONS.
**
 END-EXIT.
     EXIT.
****************************************************************
*    画面初期表示処理
****************************************************************
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE        TO  DSP-CONTROL.
     MOVE "FVS00801"    TO  DSP-FMT.
     MOVE  SPACE        TO  DSP-FVS00801.
     PERFORM  OPT-CLR-SEC.
*ＢＯＤＹ部入力へ
     MOVE  2            TO  MAIN-FLG.
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*                ＢＯＤＹ部入力処理
****************************************************************
 DSP-BODY-SEC       SECTION.
*画面表示
     MOVE      1        TO  PFK-FLG.
     PERFORM   DSP-WT-SEC.
*画面入力
     MOVE      "BODY"   TO  DSP-GRP.
     PERFORM   DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
          WHEN "E000"
                          PERFORM  CHK-BODY-SEC
                          IF    ERR-FLG = ZERO
                                MOVE    3    TO   MAIN-FLG
                          END-IF
          WHEN "F004"
                          MOVE  1      TO  MAIN-FLG
          WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
                          MOVE  4010   TO  PROGRAM-STATUS
          WHEN "F006"
                          CONTINUE
          WHEN OTHER
                          MOVE  1      TO  ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*                ＢＯＤＹ部入力チェック
****************************************************************
 CHK-BODY-SEC   SECTION.
*送受信グループ
     IF  DSP-GRPCD  =   SPACE
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-GRPCD
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-GRPCD
         MOVE  SPACE    TO  DSP-GRPNM
         MOVE   3       TO  ERR-FLG
     ELSE
         MOVE   DSP-GRPCD       TO  GRP-F01
         PERFORM  DNGROPF1-READ-SEC
         IF     GRP-INV-FLG     =   "INV"
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-GRPCD
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-GRPCD
             MOVE  SPACE    TO  DSP-GRPNM
             MOVE   1       TO  ERR-FLG
         ELSE
             MOVE  GRP-F02  TO  DSP-GRPNM
     END-IF.
     IF   ERR-FLG           =   ZERO
          MOVE  GRP-F03     TO  DSP-DTSYU1
                                WK-DTSYU
          PERFORM  DNDATSL1-READ-SEC
          MOVE  DAT-F02     TO  DSP-DTSNM1
*
          MOVE  GRP-F04     TO  DSP-DTSYU2
                                WK-DTSYU
          PERFORM  DNDATSL1-READ-SEC
          MOVE  DAT-F02     TO  DSP-DTSNM2
*
          MOVE  GRP-F05     TO  DSP-DTSYU3
                                WK-DTSYU
          PERFORM  DNDATSL1-READ-SEC
          MOVE  DAT-F02     TO  DSP-DTSNM3
*
          MOVE  GRP-F06     TO  DSP-DTSYU4
                                WK-DTSYU
          PERFORM  DNDATSL1-READ-SEC
          MOVE  DAT-F02     TO  DSP-DTSNM4
*
          MOVE  GRP-F07     TO  DSP-DTSYU5
                                WK-DTSYU
          PERFORM  DNDATSL1-READ-SEC
          MOVE  DAT-F02     TO  DSP-DTSNM5
*
     END-IF.
 CHK-BODY-EXIT.
     EXIT.
****************************************************************
*      2.2       確認入力処理
****************************************************************
 DSP-KAKU-SEC       SECTION.
*画面表示
     MOVE  2                     TO  PFK-FLG.
     PERFORM  DSP-WT-SEC.
*画面入力
     MOVE     "KAKU"            TO  DSP-GRP.
     PERFORM  DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
         WHEN "E000"
                          PERFORM  CHK-KAKU-SEC
         WHEN "F004"
                          MOVE  1       TO   MAIN-FLG
         WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
                          MOVE  4010   TO   PROGRAM-STATUS
         WHEN "F006"
                          MOVE  2       TO   MAIN-FLG
         WHEN OTHER
                          MOVE  1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*                確認入力チェック
****************************************************************
 CHK-KAKU-SEC   SECTION.
     MOVE  ZERO      TO      ERR-FLG.
*
     IF  ERR-FLG  =  ZERO
*        IF   MAIN-FLG   =   99
*             MOVE  4010     TO   PROGRAM-STATUS
*        ELSE
              MOVE  99       TO   JYO-F01
              MOVE  "D365"   TO   JYO-F02
              PERFORM HJYOKEN-READ-SEC
              IF  JYO-INV-FLG  =  "INV"
                  MOVE  4000   TO   PROGRAM-STATUS
              ELSE
                  ADD   1         TO  JYO-F04
                  IF    JYO-F04   >   JYO-F06
                     MOVE JYO-F05 TO  JYO-F04
                  END-IF
                  REWRITE  JYO-REC
                  MOVE  SYS-DATE      TO  LINK-OUT-DATE
                  MOVE  WK-TIME(1:4)  TO  LINK-OUT-TIME
                  MOVE  DSP-GRPCD     TO  LINK-OUT-GROP
                  MOVE  JYO-F04       TO  LINK-OUT-RUNNO
              END-IF
*        END-IF
         MOVE    99   TO    MAIN-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"    TO   S-NAME.
*ＰＦキー設定
     MOVE  PF-MSG-R(PFK-FLG)     TO  DSP-PFKEY.
*エラー設定
     IF    ERR-FLG   NOT = ZERO
       MOVE  ERR-MSG-R(ERR-FLG)  TO  DSP-ERRMSG
     END-IF.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FVS00801"          TO   DSP-FMT.
     WRITE    DSP-FVS00801.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*                画面表示処理
****************************************************************
 DSP-WT-SEC       SECTION.
*ＰＦキー設定
     MOVE  PF-MSG-R(PFK-FLG)      TO  DSP-PFKEY.
*エラー設定
     IF    ERR-FLG   NOT = ZERO
       MOVE  ERR-MSG-R(ERR-FLG)   TO  DSP-ERRMSG
     END-IF.
*画面表示
     MOVE "SCREEN"               TO  DSP-GRP.
     MOVE  SPACE                 TO  DSP-PRO.
     MOVE  HEN-DATE              TO  DSP-SDATE.
     MOVE  HEN-TIME              TO  DSP-STIME.
     WRITE DSP-FVS00801.
*エラークリア
     MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-ERRMSG.
 DSP-WT-EXIT.
     EXIT.
****************************************************************
*                画面読込処理
****************************************************************
 DSP-RD-SEC        SECTION.
     MOVE "NE"                   TO  DSP-PRO.
     READ  DSPFILE.
     PERFORM       OPT-CLR-SEC.
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*                項目属性初期化
****************************************************************
 OPT-CLR-SEC        SECTION.
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-GRPCD.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-GRPCD.
 OPT-CLR-EXIT.
     EXIT.
****************************************************************
*             送受信グループマスタタ 読込み          2.1
****************************************************************
 DNGROPF1-READ-SEC      SECTION.
     MOVE     "DNGROPF1-READ-SEC"   TO   S-NAME.
*
     READ      DNGROPF  INVALID
               MOVE     "INV"      TO   GRP-INV-FLG
               NOT  INVALID
               MOVE     SPACE      TO   GRP-INV-FLG
     END-READ.
*
 DNGROPF1-READ-EXIT.
     EXIT.
****************************************************************
*             送受信データ種別マスタ 読込み          2.1
****************************************************************
 DNDATSL1-READ-SEC      SECTION.
     MOVE     "DNDATSL1-READ-SEC"   TO   S-NAME.
*
     MOVE      SPACE               TO   DAT-REC.
     INITIALIZE                         DAT-REC.
     MOVE      WK-DTSYU            TO   DAT-F01.
     READ      DNDATSL1  INVALID
               MOVE     "INV"      TO   DAT-INV-FLG
               NOT  INVALID
               MOVE     SPACE      TO   DAT-INV-FLG
     END-READ.
*
 DNDATSL1-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル読込
****************************************************************
 HJYOKEN-READ-SEC       SECTION.
*
     MOVE     "HJYOKEN-READ-SEC"  TO   S-NAME.
*
     READ     HJYOKEN
         INVALID     MOVE  "INV"  TO   JYO-INV-FLG
         NOT INVALID MOVE  SPACE  TO   JYO-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*****************<<  NVD0430I   END PROGRAM  >>******************

```
