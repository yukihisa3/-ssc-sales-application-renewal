# STN0030I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/STN0030I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　_卸ローカル運用　　　　　　　　　*
*    モジュール名　　　　：　リスト発行指示【共通】　　　　　　*
*    作成日／作成者　　　：　2021/03/11   INOUE                *
*    処理内容　　　　　　：　リスト発行条件指示　　　　　　　　*
*    　　　　　　　　　　　　パラメタ引渡し　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           STN0030I.
*                 流用:STN0010I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2021/03/11.
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
*倉庫Ｍ
     SELECT   ZSOKMS1        ASSIGN        TO  DA-01-VI-ZSOKMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SOK-F01
                             FILE STATUS   IS  SOK-ST.

****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FTN00301  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1
                       LABEL     RECORD    IS   STANDARD.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "STN0030I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FVD0430I".
*フラグ領域
 01  FLG-AREA.
     03  MAIN-FLG                 PIC  9(02)  VALUE  ZERO.
     03  PFK-FLG                  PIC  9(02)  VALUE  ZERO.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SOKKBN-FLG               PIC  9(01)  VALUE  ZERO.
     03  SOK-INV-FLG              PIC  X(03)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
 01  WRK-JYO.
     03  BK-TANADATE              PIC  9(08).
     03  BK-GENNO-0               PIC  9(06).
     03  BK-GENNO-1               PIC  9(06).
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
             VALUE NC"正しい区分を入力してください".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタ未登録です".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS   6  PIC   N(20).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー　".
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
 01  LINK-IN-LSTKIND       PIC X(01).
 01  LINK-OUT-OUTKBN       PIC X(01).
 01  LINK-OUT-SOKCDS       PIC X(02).
 01  LINK-OUT-SOKCDE       PIC X(02).
*
**************************************************************
*PROCEDURE       DIVISION.
 PROCEDURE       DIVISION  USING
                                      LINK-IN-LSTKIND
                                      LINK-OUT-OUTKBN
                                      LINK-OUT-SOKCDS
                                      LINK-OUT-SOKCDE.
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
     OPEN     INPUT     ZSOKMS1.
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
                        LINK-OUT-OUTKBN
                        LINK-OUT-SOKCDS
                        LINK-OUT-SOKCDE .
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
     CLOSE             ZSOKMS1.
**
 END-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ検索
****************************************************************
 ZSOKMS1-READ-SEC   SECTION.
*
     MOVE      "   "              TO   SOK-INV-FLG.
     READ      ZSOKMS1
         INVALID
               MOVE   "INV"       TO   SOK-INV-FLG
         NOT INVALID
               MOVE   "   "       TO   SOK-INV-FLG
     END-READ.
*
 ZSOKMS1-READ-EXIT.
     EXIT.
****************************************************************
*    画面初期表示処理
****************************************************************
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE  SPACE        TO  DSP-CONTROL.
     MOVE "FTN00301"    TO  DSP-FMT.
     MOVE  SPACE        TO  DSP-FTN00301.
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
     MOVE      "PARGRP"    TO  DSP-GRP.
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
*
     MOVE   0              TO   ERR-FLG.
*
*出力区分
     IF  DSP-OUTKBN   =    SPACE OR "1"
         CONTINUE
     ELSE
         MOVE   1          TO   ERR-FLG
         MOVE  "R"         TO   EDIT-OPTION  OF  DSP-OUTKBN
         MOVE  "C"         TO   EDIT-CURSOR  OF  DSP-OUTKBN
         GO                TO   CHK-BODY-EXIT
     END-IF.
*倉庫（開始）
     IF  DSP-SOKCDS   =    SPACE
         CONTINUE
     ELSE
         MOVE     DSP-SOKCDS   TO   SOK-F01
         PERFORM  ZSOKMS1-READ-SEC
         IF       SOK-INV-FLG  =    SPACE
                  CONTINUE
         ELSE
                  IF       ERR-FLG  =  ZERO
                           MOVE     2      TO   ERR-FLG
                  END-IF
                  MOVE  "R"   TO  EDIT-OPTION  OF  DSP-SOKCDS
                  MOVE  "C"   TO  EDIT-CURSOR  OF  DSP-SOKCDS
                  GO          TO  CHK-BODY-EXIT
         END-IF
     END-IF.
*倉庫（終了）
     IF  DSP-SOKCDE   =    SPACE
         MOVE     "99"         TO   DSP-SOKCDE
     ELSE
         MOVE     DSP-SOKCDE   TO   SOK-F01
         PERFORM  ZSOKMS1-READ-SEC
         IF       SOK-INV-FLG  =    SPACE
                  CONTINUE
         ELSE
                  IF       ERR-FLG  =  ZERO
                           MOVE     2      TO   ERR-FLG
                  END-IF
                  MOVE  "R"   TO  EDIT-OPTION  OF  DSP-SOKCDE
                  MOVE  "C"   TO  EDIT-CURSOR  OF  DSP-SOKCDE
                  GO          TO  CHK-BODY-EXIT
         END-IF
     END-IF.
*大小チェック
     IF  DSP-SOKCDS   >    DSP-SOKCDE
         MOVE   3          TO   ERR-FLG
         MOVE  "R"         TO   EDIT-OPTION  OF  DSP-SOKCDS
                                EDIT-OPTION  OF  DSP-SOKCDE
         MOVE  "C"         TO   EDIT-CURSOR  OF  DSP-SOKCDS
         GO                TO   CHK-BODY-EXIT
     END-IF.
*
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
*    MOVE     "CHKGRP"            TO  DSP-GRP.
     MOVE     "ENDCHK"            TO  DSP-GRP.
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
     IF    ERR-FLG   =       ZERO
           MOVE      DSP-OUTKBN    TO   LINK-OUT-OUTKBN
           MOVE      DSP-SOKCDS    TO   LINK-OUT-SOKCDS
           MOVE      DSP-SOKCDE    TO   LINK-OUT-SOKCDE
           DISPLAY NC"出力区分＝"  LINK-OUT-OUTKBN UPON CONS
           DISPLAY NC"開始倉庫＝"  LINK-OUT-SOKCDS UPON CONS
           DISPLAY NC"終了倉庫＝"  LINK-OUT-SOKCDE UPON CONS
*
           MOVE      99            TO   MAIN-FLG
     END-IF.
*
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
     MOVE    "FTN00301"          TO   DSP-FMT.
     WRITE    DSP-FTN00301.
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
     MOVE  WORK-FORMID           TO  DSP-FMID.
*    MOVE  HEN-DATE              TO  DSP-SDATE.
     MOVE  SYS-DATE              TO  DSP-SDATE.
*    MOVE  HEN-TIME              TO  DSP-STIME.
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.
     EVALUATE   LINK-IN-LSTKIND
       WHEN "1"
           MOVE NC"_卸予定確認リスト発行指示"     TO DSP-DSPNM
           MOVE NC"_卸予定確認リスト"             TO DSP-LSTNM
       WHEN "2"
           MOVE NC"_卸結果チェックリスト発行指示" TO DSP-DSPNM
           MOVE NC"_卸結果チェックリスト"         TO DSP-LSTNM
       WHEN "3"
           MOVE NC"_卸確定リスト発行指示"         TO DSP-DSPNM
           MOVE NC"_卸確定リスト"                 TO DSP-LSTNM
       WHEN OTHER
           MOVE NC"？？？？？？？？？？？？？？？" TO DSP-DSPNM
           MOVE NC"？？？？？？？？？？？"         TO DSP-LSTNM
     END-EVALUATE.
*
     WRITE DSP-FTN00301.
*
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
     MOVE  SPACE         TO  EDIT-CURSOR  OF  DSP-OUTKBN
                             EDIT-CURSOR  OF  DSP-SOKCDS
                             EDIT-CURSOR  OF  DSP-SOKCDE.
     MOVE "M"            TO  EDIT-OPTION  OF  DSP-OUTKBN
                             EDIT-OPTION  OF  DSP-SOKCDS
                             EDIT-OPTION  OF  DSP-SOKCDE.
 OPT-CLR-EXIT.
     EXIT.
*****************<<  STN0030I   END PROGRAM  >>******************

```
