****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　プリンタ切換                      *
*    作成日／更新日　　　：　99/09/21                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKYSPLQI.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/09/21.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*プリンタマスタ
     SELECT  SMNPRTF   ASSIGN    TO        DA-01-VI-SMNPRTL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       PRT-F01
                       FILE      STATUS    PRT-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = プリンタマスタ                                     *
****************************************************************
 FD  SMNPRTF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SMNPRTF   OF   XFDLIB
                       JOINING   PRT       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FKYSPLQI  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  PRT-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  SAV-PR-SPLQ              PIC  X(08)  VALUE  SPACE.
*ワーク領域プログラムスイッチ（画面遷移制御）
 01  WRK-AREA.
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*日付／時刻編集
 01  HEN-DATE.
     03  HEN-YYYY                 PIC   9(04)  VALUE  ZERO.
     03  FILLER                   PIC   X(01)  VALUE  "/".
     03  HEN-MM                   PIC   9(02)  VALUE  ZERO.
     03  FILLER                   PIC   X(01)  VALUE  "/".
     03  HEN-DD                   PIC   9(02)  VALUE  ZERO.
 01  HEN-TIME.
     03  HEN-T-HH                 PIC   Z9.
     03  FILLER                   PIC   X(01)  VALUE  ":".
     03  HEN-T-MM                 PIC   Z9.
     03  FILLER                   PIC   X(01)  VALUE  ":".
     03  HEN-T-SS                 PIC   Z9.
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了　　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了_項目戻し　　　　　　".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"プリンタを選択して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"正しいプリンタを選択して下さい。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  3   PIC   N(20).
*基本スケジュール退避エリア
 01  TABLE-AREA.
     03  TABLE1      OCCURS  12.
         05  TBL-SEQ             PIC   9(02).
         05  TBL-PRTNM           PIC   N(10).
         05  TBL-PRTS            PIC   X(08).
         05  TBL-PRTQ            PIC   X(08).
         05  TBL-KBN             PIC   X(01).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  PRT-ERR           PIC N(15) VALUE
                        NC"プリンタマスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
*現在選択中プリンタ退避
 01  SAV-DSP-AREA.
     03  SAV-PRTNM         PIC N(10) VALUE  SPACE.
     03  SAV-PRTS          PIC X(08) VALUE  SPACE.
     03  SAV-PRTQ          PIC X(08) VALUE  SPACE.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
 LINKAGE               SECTION.
 01  LINK-IN-SPLQ          PIC X(08).
 01  LINK-OUT-SPLQ         PIC X(08).
****************************************************************
 PROCEDURE       DIVISION  USING   LINK-IN-SPLQ   LINK-OUT-SPLQ.
****************************************************************
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SMNPRTF.
     MOVE        PRT-ST    TO        E-ST.
     MOVE        "SMNPRTF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     PRT-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
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
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      SYS-DATE(1:4)      TO   HEN-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DD.
     MOVE      WK-TIME(1:2)       TO   HEN-T-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-T-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-T-SS.
*ファイルのＯＰＥＮ
     OPEN      INPUT            SMNPRTF.
     OPEN      I-O              DSPFILE.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*ﾊﾟﾗﾒﾀINﾊﾞｯｸｱｯﾌﾟ
     MOVE      LINK-IN-SPLQ       TO   SAV-PR-SPLQ.
*****DISPLAY "LINK-IN-SPLQ  = " LINK-IN-SPLQ  UPON CONS.
*****DISPLAY "LINK-OUT-SPLQ = " LINK-OUT-SPLQ UPON CONS.
*プリンタマスタの表示の為、プリンタマスタスタート
     MOVE      SPACE            TO      PRT-F01.
     START     SMNPRTF  KEY  IS  >=     PRT-F01
               INVALID
                 DISPLAY "**ﾌﾟﾘﾝﾀﾏｽﾀ ﾄｳﾛｸﾅｼ      **" UPON CONS
                 DISPLAY "**ｶﾞﾒﾝ ﾆ ﾋｮｳｼﾞ ﾃﾞｷﾏｾﾝ  **" UPON CONS
                 DISPLAY "**ﾌﾟﾛｸﾞﾗﾑ ｦ ｼｭｳﾘｮｳ ｼﾏｽ **" UPON CONS
                 MOVE     "END"     TO   END-FLG
                 GO                 TO   INIT-EXIT
               NOT  INVALID
                 PERFORM   SMNPRTF-READ-SEC
                 IF  RD-FLG =  "END"
                     DISPLAY "**ﾌﾟﾘﾝﾀﾏｽﾀ ﾄｳﾛｸﾅｼ      **" UPON CONS
                     DISPLAY "**ｶﾞﾒﾝ ﾆ ﾋｮｳｼﾞ ﾃﾞｷﾏｾﾝ  **" UPON CONS
                     DISPLAY "**ﾌﾟﾛｸﾞﾗﾑ ｦ ｼｭｳﾘｮｳ ｼﾏｽ **" UPON CONS
                     MOVE     "END"     TO   END-FLG
                     GO                 TO   INIT-EXIT
                 END-IF
     END-START.
*ワークテーブルへプリンタ情報セット
     PERFORM    PRTF-JYOHO-SEC.
*初期画面表示へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*初期画面表示（プリンタ情報取得）
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*プリンタ選択
         WHEN      "2"       PERFORM   DSP-BODY-SEC
*確認入力
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
*以外
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"          TO   S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE   SMNPRTF.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示 (PSW = 1)                           *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*初期画面の表示
     MOVE    SPACE                TO   DSP-PRO.
*画面の初期化
     MOVE    SPACE                TO   DSP-FKYSPLQI.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*プログラムＩＤ
     MOVE    "SKYSPLQI"           TO   DSP-SID.
*フォームＩＤ
     MOVE    "FKYSPLQI"           TO   DSP-FID.
*現在プリンタ名
     MOVE    SAV-PRTNM            TO   DSP-GPRTNM.
*現在プリンタ装置名
     MOVE    SAV-PRTS             TO   DSP-GPRTS.
*現在プリンタキュー
     MOVE    SAV-PRTQ             TO   DSP-GPRTQ.
*画面表示（ワーク→画面）
     PERFORM  WORK-TO-DSP-SEC.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*初期画面表示へ
     MOVE     "2"                 TO    PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 2 )              2.2       *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"       TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM BODY-CHK-SEC
                IF  ERR-FLG  =  ZERO
                    MOVE    "3"    TO    PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             プリンタ選択チェック                   2.2.1     *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE     "BODY-CHK-SEC"      TO   S-NAME.
*プリンタ選択チェック
*未入力チェック
     IF       DSP-SENPRT  NOT  NUMERIC
     OR       DSP-SENPRT  =  ZERO
              MOVE   1    TO ERR-FLG
              MOVE "R"    TO EDIT-OPTION OF DSP-SENPRT
              MOVE "C"    TO EDIT-CURSOR OF DSP-SENPRT
              GO          TO BODY-CHK-EXIT
     ELSE
              MOVE "M"    TO EDIT-OPTION OF DSP-SENPRT
              MOVE SPACE  TO EDIT-CURSOR OF DSP-SENPRT
     END-IF.
*ＳＥＱ入力範囲チェック
     IF       DSP-SENPRT  >  12
              MOVE   2    TO ERR-FLG
              MOVE "R"    TO EDIT-OPTION OF DSP-SENPRT
              MOVE "C"    TO EDIT-CURSOR OF DSP-SENPRT
              GO          TO BODY-CHK-EXIT
     ELSE
              MOVE "M"    TO EDIT-OPTION OF DSP-SENPRT
              MOVE  SPACE TO EDIT-CURSOR OF DSP-SENPRT
     END-IF.
*使用可能プリンタチェック
     IF       TBL-KBN(DSP-SENPRT)  NOT =  "1"
              MOVE   2    TO ERR-FLG
              MOVE "R"    TO EDIT-OPTION OF DSP-SENPRT
              MOVE "C"    TO EDIT-CURSOR OF DSP-SENPRT
     ELSE
              MOVE "M"    TO EDIT-OPTION OF DSP-SENPRT
              MOVE  SPACE TO EDIT-CURSOR OF DSP-SENPRT
     END-IF.
*
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 3 ）            2.4
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE     "DSP-KAKU-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    SPACE                 TO LINK-OUT-SPLQ
                MOVE    TBL-PRTQ(DSP-SENPRT)  TO LINK-OUT-SPLQ
*********DISPLAY "OUT-PRTQ = " TBL-PRTQ(DSP-SENPRT) UPON CONS
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
*曜日
         WHEN   "1"   WHEN   "2"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
*明細／確認
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FKYSPLQI"          TO   DSP-FMT.
     WRITE    DSP-FKYSPLQI.
     PERFORM  DSP-SYOKI-SEC.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
*
     MOVE    "NE"                 TO   DSP-PRO.
*
     EVALUATE   PSW
*明細
         WHEN   "2"
                MOVE    "SEN"     TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FKYSPLQI"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "DSP-SYOKI-SEC"    TO   S-NAME.
*リバース，カーソルパーク解除
***  プリンタ選択
     MOVE "M"   TO EDIT-OPTION   OF   DSP-SENPRT.
     MOVE SPACE TO EDIT-CURSOR   OF   DSP-SENPRT.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-TO-DSP-SEC       SECTION.
     MOVE     "WORK-TO-DSP-SEC"   TO   S-NAME.
*項目画面セット
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 12
             MOVE   TBL-SEQ(X)   TO DSP-SSEQ(X)
             IF  TBL-KBN(X)  =  "1"
                 MOVE   TBL-PRTNM(X) TO DSP-SPRTNM(X)
                 MOVE   TBL-PRTS(X)  TO DSP-SSOTI(X)
                 MOVE   TBL-PRTQ(X)  TO DSP-SKYU(X)
             END-IF
     END-PERFORM.
*
 WORK-TO-DSP-EXIT.
     EXIT.
****************************************************************
*             プリンタマスタ（順）読込み             3.0       *
****************************************************************
 SMNPRTF-READ-SEC      SECTION.
*
     MOVE     "SMNPRTF-READ-SEC"  TO   S-NAME.
*プリンタマスタ読込み
     READ   SMNPRTF  AT  END
            MOVE     "END"        TO   RD-FLG
     END-READ.
*
 SMNPRTF-READ-EXIT.
     EXIT.
****************************************************************
*             画面→ワーク退避                                 *
****************************************************************
 PRTF-JYOHO-SEC        SECTION.
     MOVE     "PRTF-JYOHO-SEC"    TO   S-NAME.
*
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 12
             MOVE   X             TO   TBL-SEQ(X)
             IF     RD-FLG  NOT = "END"
                    MOVE PRT-F03  TO   TBL-PRTNM(X)
                    MOVE PRT-F02  TO   TBL-PRTS(X)
                    MOVE PRT-F01  TO   TBL-PRTQ(X)
                    MOVE "1"      TO   TBL-KBN(X)
******DISPLAY "PRT-F01      = " PRT-F01      UPON CONS
******DISPLAY "SAV-PR-SPLQ  = " SAV-PR-SPLQ  UPON CONS
                    IF   PRT-F01 = SAV-PR-SPLQ
*************************DISPLAY "A" UPON CONS
                         MOVE   PRT-F03  TO  SAV-PRTNM
                         MOVE   PRT-F02  TO  SAV-PRTS
                         MOVE   PRT-F01  TO  SAV-PRTQ
                    END-IF
                    PERFORM SMNPRTF-READ-SEC
             END-IF
     END-PERFORM.
*
 PRTF-JYOHO-EXIT.
     EXIT.
*****************<<  SKYSPLQI   END PROGRAM  >>******************
