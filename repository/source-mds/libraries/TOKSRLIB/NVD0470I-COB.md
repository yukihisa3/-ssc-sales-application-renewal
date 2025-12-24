# NVD0470I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0470I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　倉庫間在庫移動入力ﾁｪｯｸﾘｽﾄ出力指示
*    作成日／作成者　　　：　2020/04/09   ASS.II               *
*    処理内容　　　　　　：　倉庫間在庫移動入力ﾁｪｯｸﾘｽﾄ発行条件
*    　　　　　　　　　　　　を画面より指定する。　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVD0470I.
 AUTHOR.               ASS.II.
 DATE-WRITTEN.         20/04/09.
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
     SELECT  DSPFILE   ASSIGN        TO  GS-DSPF
                       FORMAT            DSP-FMT
                       GROUP             DSP-GRP
                       PROCESSING        DSP-PRO
                       FUNCTION          DSP-FNC
                       FILE STATUS       DSP-ST.
*
*倉庫Ｍ
     SELECT   ZSOKMS1  ASSIGN        TO  DA-01-VI-ZSOKMS1
                       ORGANIZATION  IS  INDEXED
                       ACCESS MODE   IS  RANDOM
                       RECORD KEY    IS  SOK-F01
                       FILE STATUS   IS  SOK-ST.

*担当者Ｍ
     SELECT   HTANMS   ASSIGN        TO  DA-01-VI-TANMS1
                       ORGANIZATION  IS  INDEXED
                       ACCESS MODE   IS  RANDOM
                       RECORD KEY    IS  TAN-F01
                                         TAN-F02
                       FILE STATUS   IS  TAN-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE.
                       COPY      FVD04701  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1
                       LABEL     RECORD    IS   STANDARD.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*担当者Ｍ
 FD  HTANMS             LABEL     RECORD     IS  STANDARD.
     COPY     HTANMS    OF   XFDLIB
     JOINING   TAN  AS PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "NVD0470I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FVD0470I".
*フラグ領域
 01  FLG-AREA.
     03  MAIN-FLG                 PIC  9(02)  VALUE  ZERO.
     03  PFK-FLG                  PIC  9(02)  VALUE  ZERO.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SOKKBN-FLG               PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
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
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
 01  WK-SDATE                PIC  9(08).
 01  WK-SDATE-R   REDEFINES   WK-SDATE.
     03  FILLER              PIC  X(02).
     03  WK-SYMD.
       05  WK-SYY            PIC  9(02).
       05  WK-SMM            PIC  9(02).
       05  WK-SDD            PIC  9(02).
 01  WK-EDATE                PIC  9(08).
 01  WK-EDATE-R   REDEFINES   WK-EDATE.
     03  FILLER              PIC  X(02).
     03  WK-EYMD.
       05  WK-EYY            PIC  9(02).
       05  WK-EMM            PIC  9(02).
       05  WK-EDD            PIC  9(02).
 01  WK-SDEN                 PIC  9(07)  VALUE  ZERO.
 01  WK-EDEN                 PIC  9(07)  VALUE  ZERO.
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
             VALUE NC"倉庫マスタ未登録です。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"担当者マスタ未登録です。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"日付エラーです。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS   6  PIC   N(20).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  TAN-ERR           PIC N(20) VALUE
                        NC"担当者マスタエラー".
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
 01  LINK-IN-BUMON         PIC X(04).
 01  LINK-IN-TANTO         PIC X(02).
 01  LINK-IN-SOKO          PIC X(02).
 01  LINK-OUT-TANTO        PIC X(02).
 01  LINK-OUT-SOKO         PIC X(02).
 01  LINK-OUT-SHIJIST      PIC 9(08).
 01  LINK-OUT-SHIJIEN      PIC 9(08).
 01  LINK-OUT-DENNOST      PIC 9(07).
 01  LINK-OUT-DENNOEN      PIC 9(07).
*
**************************************************************
 PROCEDURE       DIVISION  USING
                                     LINK-IN-BUMON
                                     LINK-IN-TANTO
                                     LINK-IN-SOKO
                                     LINK-OUT-TANTO
                                     LINK-OUT-SOKO
                                     LINK-OUT-SHIJIST
                                     LINK-OUT-SHIJIEN
                                     LINK-OUT-DENNOST
                                     LINK-OUT-DENNOEN.
**************************************************************
 DECLARATIVES.
 DSP-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*倉庫Ｍ
 SOK-ERR-SEC               SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*担当者Ｍ
 TAN-ERR-SEC               SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       HTANMS.
     DISPLAY     TAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
******************* TEST
*    MOVE    "2920"               TO   LINK-IN-BUMON.
*    MOVE    "01"                 TO   LINK-IN-TANTO.
*    MOVE    "02"                 TO   LINK-IN-SOKO.
     DISPLAY "IN 部　門  ="  LINK-IN-BUMON     UPON CONS.
     DISPLAY "IN 担当者  ="  LINK-IN-TANTO     UPON CONS.
     DISPLAY "IN 出庫倉庫="  LINK-IN-SOKO      UPON CONS.
******************* TEST
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
     OPEN     INPUT     ZSOKMS1.
     OPEN     INPUT     HTANMS.
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
*ワークの初期化
     INITIALIZE         FLG-AREA
                        LINK-OUT-TANTO
                        LINK-OUT-SOKO
                        LINK-OUT-SHIJIST
                        LINK-OUT-SHIJIEN
                        LINK-OUT-DENNOST
                        LINK-OUT-DENNOEN.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    TIME-AREA          FROM   TIME.
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
     CLOSE             ZSOKMS1.
     CLOSE             HTANMS.
******************* TEST
     DISPLAY "指示担当者  ="  LINK-OUT-TANTO     UPON CONS.
     DISPLAY "出庫倉庫    ="  LINK-OUT-SOKO      UPON CONS.
     DISPLAY "指示日開始  ="  LINK-OUT-SHIJIST   UPON CONS.
     DISPLAY "指示日終了  ="  LINK-OUT-SHIJIEN   UPON CONS.
     DISPLAY "伝票番号開始="  LINK-OUT-DENNOST   UPON CONS.
     DISPLAY "伝票番号終了="  LINK-OUT-DENNOEN   UPON CONS.
     DISPLAY "PROGRAM-STATUS="  PROGRAM-STATUS   UPON CONS.
******************* TEST
**
 END-EXIT.
     EXIT.
****************************************************************
*    画面初期表示処理
****************************************************************
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE        TO  DSP-CONTROL.
     MOVE "FVD04701"    TO  DSP-FMT.
     MOVE  SPACE        TO  DSP-FVD04701.
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
     MOVE      "GRP001" TO  DSP-GRP.
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
*倉庫ＣＤ
     IF  DSP-SOKCD   =   SPACE
         MOVE   NC"全倉庫"       TO  DSP-SOKNM
     ELSE
         MOVE   DSP-SOKCD        TO  SOK-F01
         READ   ZSOKMS1
             INVALID  KEY
                 IF  ERR-FLG  =  ZERO
                     MOVE   1        TO  ERR-FLG
                 END-IF
                 MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SOKCD
                 MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SOKCD
                 MOVE  SPACE      TO  DSP-SOKNM
             NOT  INVALID  KEY
                 MOVE  SOK-F02    TO  DSP-SOKNM
         END-READ
     END-IF.
*担当者ＣＤ
     IF  DSP-TANCD   =   SPACE
         MOVE   NC"全担当者"     TO  DSP-TANNM
     ELSE
         MOVE   LINK-IN-BUMON    TO  TAN-F01
         MOVE   DSP-TANCD        TO  TAN-F02
         READ   HTANMS
             INVALID  KEY
                 IF  ERR-FLG  =  ZERO
                     MOVE   2        TO  ERR-FLG
                 END-IF
                 MOVE  "R"       TO  EDIT-OPTION  OF  DSP-TANCD
                 MOVE  "C"       TO  EDIT-CURSOR  OF  DSP-TANCD
                 MOVE  SPACE     TO  DSP-TANNM
             NOT  INVALID  KEY
                 MOVE  TAN-F03   TO  DSP-TANNM
         END-READ
     END-IF.
*開始指示日
     IF  DSP-FDATE   NOT NUMERIC
         MOVE     ZERO            TO   DSP-FDATE
     END-IF.
     IF  DSP-FDATE   NOT =  ZERO
         MOVE     "2"                 TO   LINK-IN-KBN
         MOVE     DSP-FDATE           TO   LINK-IN-YMD8
         CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8
         IF  LINK-OUT-RET      =  ZERO
             MOVE     DSP-FDATE       TO   WK-SDATE
         ELSE
             MOVE     ZERO            TO   WK-SDATE
             IF  ERR-FLG  =  ZERO
                 MOVE   3        TO  ERR-FLG
             END-IF
             MOVE  "R"           TO  EDIT-OPTION  OF  DSP-FDATE
             MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-FDATE
         END-IF
     ELSE
             MOVE     ZERO            TO   WK-SDATE
     END-IF.
*終了指示日
     IF  DSP-TDATE   NOT NUMERIC
         MOVE     ZERO            TO   DSP-TDATE
     END-IF.
     IF  DSP-TDATE   NOT =  ZERO      AND
         DSP-TDATE   NOT =  99999999
         MOVE     "2"                 TO   LINK-IN-KBN
         MOVE     DSP-TDATE           TO   LINK-IN-YMD8
         CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8
         IF  LINK-OUT-RET   =  ZERO
             MOVE     DSP-TDATE       TO   WK-EDATE
         ELSE
             MOVE     ZERO            TO   WK-EDATE
             IF  ERR-FLG  =  ZERO
                 MOVE   3        TO  ERR-FLG
             END-IF
             MOVE  "R"           TO  EDIT-OPTION  OF  DSP-TDATE
             MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-TDATE
         END-IF
     ELSE
         MOVE  99999999      TO  DSP-TDATE
         MOVE  99999999      TO  WK-EDATE
     END-IF.
*開始・終了の関連チェック
     IF  WK-SDATE  NOT =  ZERO  AND
         WK-EDATE  NOT =  ZERO  AND
         WK-SDATE     >   WK-EDATE
         IF  ERR-FLG  =  ZERO
             MOVE   4        TO  ERR-FLG
*      DISPLAY "AAA" UPON CONS
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-FDATE
                                 EDIT-OPTION  OF  DSP-TDATE
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-FDATE
     END-IF.
*伝票番号関連チェック
     MOVE  ZERO              TO  WK-SDEN  WK-EDEN.
     IF    DSP-DENST  NUMERIC
           MOVE  DSP-DENST         TO  WK-SDEN
     ELSE
           MOVE  ZERO              TO  WK-SDEN
     END-IF.
     IF    DSP-DENED     =   ZERO
           MOVE  9999999     TO  WK-EDEN
     ELSE
           MOVE  DSP-DENED   TO  WK-EDEN
     END-IF.
     IF    WK-EDEN       <   WK-SDEN
         IF  ERR-FLG     =   ZERO
             MOVE  4     TO  ERR-FLG
         END-IF
         MOVE  "R"           TO  EDIT-OPTION  OF  DSP-DENST
                                 EDIT-OPTION  OF  DSP-DENED
         MOVE  "C"           TO  EDIT-CURSOR  OF  DSP-DENST
     END-IF.
     MOVE    WK-SDEN         TO  DSP-DENST.
     MOVE    WK-EDEN         TO  DSP-DENED.
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
     MOVE     "ENDCHK"           TO  DSP-GRP.
     PERFORM  DSP-RD-SEC.
*ＰＦ判定
     EVALUATE  DSP-FNC
         WHEN "E000"
                          PERFORM  CHK-KAKU-SEC
         WHEN "F004"
                          MOVE  1       TO   MAIN-FLG
         WHEN "F005"
                          MOVE  99     TO  MAIN-FLG
                          MOVE  4010   TO  PROGRAM-STATUS
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
         MOVE  DSP-TANCD  TO   LINK-OUT-TANTO
         MOVE  DSP-SOKCD  TO   LINK-OUT-SOKO
         MOVE  DSP-FDATE  TO   LINK-OUT-SHIJIST
         IF    DSP-TDATE  =    ZERO
               MOVE "99999999" TO   LINK-OUT-SHIJIEN
         ELSE
               MOVE DSP-TDATE  TO   LINK-OUT-SHIJIEN
         END-IF
         MOVE  DSP-DENST  TO   LINK-OUT-DENNOST
         MOVE  DSP-DENED  TO   LINK-OUT-DENNOEN
         MOVE  99         TO   MAIN-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"    TO   S-NAME.
*ＰＦキー設定
     MOVE  PF-MSG-R(PFK-FLG)     TO  DSP-FNCSPC.
*エラー設定
     IF    ERR-FLG   NOT = ZERO
       MOVE  ERR-MSG-R(ERR-FLG)  TO  DSP-MSGSPC
     END-IF.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FVD04701"          TO   DSP-FMT.
     WRITE    DSP-FVD04701.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*                画面表示処理
****************************************************************
 DSP-WT-SEC       SECTION.
*ＰＦキー設定
     MOVE  PF-MSG-R(PFK-FLG)      TO  DSP-FNCSPC.
*エラー設定
     IF    ERR-FLG   NOT = ZERO
       MOVE  ERR-MSG-R(ERR-FLG)   TO  DSP-MSGSPC
     END-IF.
*画面表示
     MOVE "SCREEN"               TO  DSP-GRP.
     MOVE  SPACE                 TO  DSP-PRO.
     MOVE  WORK-PGID             TO  DSP-PGID.
     MOVE  WORK-FORMID           TO  DSP-FORMID.
     MOVE  SYS-DATE              TO  DSP-SDATE.
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.
     WRITE DSP-FVD04701.
*エラークリア
     MOVE  ZERO                  TO  ERR-FLG.
     MOVE  SPACE                 TO  DSP-MSGSPC.
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
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-TANCD
                             EDIT-CURSOR  OF  DSP-SOKCD
                             EDIT-CURSOR  OF  DSP-FDATE
                             EDIT-CURSOR  OF  DSP-TDATE
                             EDIT-CURSOR  OF  DSP-DENST
                             EDIT-CURSOR  OF  DSP-DENED.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-TANCD
                             EDIT-OPTION  OF  DSP-SOKCD
                             EDIT-OPTION  OF  DSP-FDATE
                             EDIT-OPTION  OF  DSP-TDATE
                             EDIT-OPTION  OF  DSP-DENST
                             EDIT-OPTION  OF  DSP-DENED.
 OPT-CLR-EXIT.
     EXIT.
*****************<<  NVD0470I   END PROGRAM  >>******************

```
