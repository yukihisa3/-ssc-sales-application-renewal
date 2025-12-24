****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　システム共通サブシステム　　　　　*
*    モジュール名　　　　：　連携エラーリカバリリスト発行指示　*
*    作成日／作成者　　　：　2021/11/17 INOUE                  *
*    処理概要　　　　　　：　リスト発行指示　　　　　　　　　　*
*    　　　　　　　　　　：　　　　　　                        *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SER0110I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2021/11/17.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
*担当者マスタ
     SELECT  HTANMS    ASSIGN    TO        TANMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TAN-F01
                                           TAN-F02
                       FILE      STATUS    TAN-ST.
*Ｄ３６５累積ファイル３
     SELECT  ERRRUIL3  ASSIGN    TO        ERRRUIL3
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       ERR3-F01
                                           ERR3-F043
                       FILE      STATUS    ERR3-ST.
*Ｄ３６５累積ファイル４
     SELECT  ERRRUIL4  ASSIGN    TO        ERRRUIL4
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       ERR4-F02
                                           ERR4-F03
                                           ERR4-F043
                       FILE      STATUS    ERR4-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FER01101  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
*    FILE = 担当者マスタ                                       *
****************************************************************
 FD  HTANMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTANMS    OF   XFDLIB
                       JOINING   TAN       AS   PREFIX.
****************************************************************
*  FILE= Ｄ３６５累積ファイル3                               *
****************************************************************
 FD  ERRRUIL3
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ERRRUIL3  OF   XFDLIB
                       JOINING   ERR3      AS   PREFIX.
****************************************************************
*  FILE= Ｄ３６５累積ファイル4                               *
****************************************************************
 FD  ERRRUIL4
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ERRRUIL4  OF   XFDLIB
                       JOINING   ERR4      AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
     03  ERR3-ST                  PIC  X(02).
     03  ERR4-ST                  PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SER0110I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FER01101".
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SAKKBN-FLG               PIC  9(01)  VALUE  ZERO.
     03  ERRRUIL3-END             PIC  X(03)  VALUE  SPACE.
     03  ERRRUIL4-END             PIC  X(03)  VALUE  SPACE.
*    03  ERR3-INV-FLG             PIC  X(03)  VALUE  SPACE.
*    03  ERR4-INV-FLG             PIC  X(03)  VALUE  SPACE.
     03  TAN-INV-FLG              PIC  X(03)  VALUE  SPACE.
     03  ERR1-FLG                 PIC  X(03)  VALUE  SPACE.
     03  ERR2-FLG                 PIC  X(03)  VALUE  SPACE.
     03  ERR3-FLG                 PIC  X(03)  VALUE  SPACE.
     03  ERR4-FLG                 PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  RD-CNT                       PIC  9(08)  VALUE  ZERO.
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
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
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
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
             VALUE NC"空白またはＹのみ有効です".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"空白または１のみ有効です".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（日付）論理エラー".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（時間）論理エラー".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"管理番号を入力してください".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"Ｄ３６５累積ファイルに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい出力区分を指定してください".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチＮＯを入力してください".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  9   PIC   N(20).
*
 01  FILE-ERR.
     03  TAN-ERR           PIC N(20) VALUE
                        NC"担当者マスタエラー".
     03  ERR3-ERR          PIC N(20) VALUE
                        NC"Ｄ３６５累積ファイルエラー".
     03  ERR4-ERR          PIC N(20) VALUE
                        NC"Ｄ３６５累積ファイルエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-IN-BUMCD       PIC  X(04).
 01  LINK-IN-TANCD       PIC  X(02).
 01  LINK-OUT-ERR1       PIC  X(01).
 01  LINK-OUT-ERR2       PIC  X(01).
 01  LINK-OUT-ERR3       PIC  X(01).
 01  LINK-OUT-ERR4       PIC  X(01).
 01  LINK-OUT-SEL        PIC  X(01).
 01  LINK-OUT-KANRINO    PIC  9(08).
 01  LINK-OUT-BDATE      PIC  9(08).
 01  LINK-OUT-BTIME      PIC  9(06).
 01  LINK-OUT-OUTPUT     PIC  X(01).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-IN-BUMCD
                                     LINK-IN-TANCD
                                     LINK-OUT-ERR1
                                     LINK-OUT-ERR2
                                     LINK-OUT-ERR3
                                     LINK-OUT-ERR4
                                     LINK-OUT-SEL
                                     LINK-OUT-KANRINO
                                     LINK-OUT-BDATE
                                     LINK-OUT-BTIME
                                     LINK-OUT-OUTPUT.
**************************************************************
 DECLARATIVES.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ERR3-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ERRRUIL3.
     DISPLAY     ERR3-ERR  UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR3-ST   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ERR4-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ERRRUIL4.
     DISPLAY     ERR4-ERR  UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR4-ST   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTANMS  ERRRUIL3  ERRRUIL4.
*ワークの初期化
     INITIALIZE   FLG-AREA.
     INITIALIZE   LINK-OUT-ERR1       LINK-OUT-KANRINO
                  LINK-OUT-ERR2       LINK-OUT-BDATE
                  LINK-OUT-ERR3       LINK-OUT-BTIME
                  LINK-OUT-ERR4       LINK-OUT-OUTPUT
                  LINK-OUT-SEL.
*
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
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
*パラメタ入力1
         WHEN      "1"  PERFORM   DSP-PARA1-SEC
*パラメタ入力2
         WHEN      "2"  PERFORM   DSP-PARA2-SEC
*パラメタ入力3
         WHEN      "3"  PERFORM   DSP-PARA3-SEC
*パラメタ入力4
         WHEN      "4"  PERFORM   DSP-PARA4-SEC
*確認入力
         WHEN      "K"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-PARA1-SEC         SECTION.
     MOVE     "DSP-PARA1-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK1-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-PARA1-SEC
     END-EVALUATE.
*
 DSP-PARA1-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック(PSW=1)                2.1.1     *
****************************************************************
 PARA-CHK1-SEC             SECTION.
     MOVE     "PARA-CHK1-SEC"     TO   S-NAME.
*
*対象データ選択
     IF      ( DSP-ERR1  NOT =  " " ) AND
             ( DSP-ERR1  NOT =  "Y" )
               MOVE   1       TO   ERR-FLG
               MOVE  "R"      TO   EDIT-OPTION  OF  DSP-ERR1
               MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-ERR1
     ELSE
               MOVE  "M"      TO   EDIT-OPTION  OF  DSP-ERR1
               MOVE  " "      TO   EDIT-CURSOR  OF  DSP-ERR1
     END-IF.
*
     IF      ( DSP-ERR2  NOT =  " " ) AND
             ( DSP-ERR2  NOT =  "Y" )
               MOVE  "R"      TO   EDIT-OPTION  OF  DSP-ERR2
               IF    ERR-FLG  = 0
                     MOVE   1       TO   ERR-FLG
                     MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-ERR2
               END-IF
     ELSE
               MOVE  "M"      TO   EDIT-OPTION  OF  DSP-ERR2
               MOVE  " "      TO   EDIT-CURSOR  OF  DSP-ERR2
     END-IF.
*
     IF      ( DSP-ERR3  NOT =  " " ) AND
             ( DSP-ERR3  NOT =  "Y" )
               MOVE  "R"      TO   EDIT-OPTION  OF  DSP-ERR3
               IF    ERR-FLG  = 0
                     MOVE   1       TO   ERR-FLG
                     MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-ERR3
               END-IF
     ELSE
               MOVE  "M"      TO   EDIT-OPTION  OF  DSP-ERR3
               MOVE  " "      TO   EDIT-CURSOR  OF  DSP-ERR3
     END-IF.
*
     IF      ( DSP-ERR4  NOT =  " " ) AND
             ( DSP-ERR4  NOT =  "Y" )
               MOVE  "R"      TO   EDIT-OPTION  OF  DSP-ERR4
               IF    ERR-FLG  = 0
                     MOVE   1       TO   ERR-FLG
                     MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-ERR4
               END-IF
     ELSE
               MOVE  "M"      TO   EDIT-OPTION  OF  DSP-ERR4
               MOVE  " "      TO   EDIT-CURSOR  OF  DSP-ERR4
     END-IF.
*
*選択　チェック
     IF      ( DSP-SENTAK   NOT =  " " ) AND
             ( DSP-SENTAK   NOT =  "1" )
               MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SENTAK
               IF    ERR-FLG  = 0
                     MOVE   2      TO   ERR-FLG
                     MOVE  "C"     TO   EDIT-CURSOR OF DSP-SENTAK
               END-IF
     ELSE
               MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SENTAK
               MOVE  " "      TO   EDIT-CURSOR  OF  DSP-SENTAK
     END-IF.
*
*遷移先制御
     IF        ERR-FLG   NOT =  0
               GO             TO   PARA-CHK1-EXIT
     END-IF.
     IF        DSP-SENTAK       =  " "
               MOVE    "2"    TO   PSW
     ELSE
               MOVE    "3"    TO   PSW
     END-IF.
*
 PARA-CHK1-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 2 )                2.1       *
****************************************************************
 DSP-PARA2-SEC         SECTION.
     MOVE     "DSP-PARA2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK2-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*戻り
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*               PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-PARA2-SEC
     END-EVALUATE.
*
 DSP-PARA2-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック(PSW=2)                2.1.1     *
****************************************************************
 PARA-CHK2-SEC             SECTION.
     MOVE     "PARA-CHK2-SEC"     TO   S-NAME.
*
 PARA-CHK2-01.
*管理番号　チェック
     IF       DSP-KANRNO   NOT NUMERIC
         OR   DSP-KANRNO   =  ZERO
              MOVE   6       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KANRNO
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KANRNO
              GO             TO   PARA-CHK2-EXIT
     END-IF.
 PARA-CHK2-02.
*累積ファイル　存在チェック（ＡＬＬ" " )
     IF     ( DSP-ERR1 =  " " ) AND
            ( DSP-ERR2 =  " " ) AND
            ( DSP-ERR3 =  " " ) AND
            ( DSP-ERR4 =  " " )
              MOVE     DSP-KANRNO      TO     ERR3-F01
              MOVE     "01"             TO     ERR3-F043
              START    ERRRUIL3 KEY IS  >=     ERR3-F01 ERR3-F043
                  INVALID
                       MOVE  "R"   TO  EDIT-OPTION OF DSP-KANRNO
                       IF    ERR-FLG   = 0
                             MOVE   7     TO   ERR-FLG
                             MOVE  "C"
                                    TO EDIT-CURSOR OF DSP-KANRNO
                       END-IF
                       GO               TO     PARA-CHK2-EXIT
              END-START
              PERFORM  ERRRUIL3-READ-SEC
              IF       ERRRUIL3-END  =  "END"
                       MOVE  "R"   TO  EDIT-OPTION OF DSP-KANRNO
                       IF    ERR-FLG   = 0
                             MOVE   7     TO   ERR-FLG
                             MOVE  "C"
                                    TO EDIT-CURSOR OF DSP-KANRNO
                       END-IF
                       GO               TO     PARA-CHK2-EXIT
              END-IF
              IF     ( ERR3-F01    =   DSP-KANRNO ) AND
                     (( ERR3-F043  =   "01" ) OR
                      ( ERR3-F043  =   "02" ) OR
                      ( ERR3-F043  =   "03" ) OR
                      ( ERR3-F043  =   "04" ) OR
                      ( ERR3-F043  =   "05" ))
                       MOVE  "M"   TO  EDIT-OPTION OF DSP-KANRNO
                       MOVE  " "   TO  EDIT-CURSOR OF DSP-KANRNO
                       MOVE ERR3-F02   TO     LINK-OUT-BDATE
                       MOVE ERR3-F03   TO     LINK-OUT-BTIME
                       GO          TO  PARA-CHK2-10
              END-IF
     END-IF.
 PARA-CHK2-03.
*累積ファイル　存在チェック
     MOVE     DSP-KANRNO      TO     ERR3-F01.
     MOVE     "01"             TO     ERR3-F043
     START    ERRRUIL3 KEY IS  >=     ERR3-F01 ERR3-F043
         INVALID
              MOVE  "R"   TO  EDIT-OPTION OF DSP-KANRNO
              IF    ERR-FLG   = 0
                    MOVE   7     TO   ERR-FLG
                    MOVE  "C"    TO   EDIT-CURSOR OF DSP-KANRNO
              END-IF
              GO                 TO   PARA-CHK2-EXIT
     END-START.
     PERFORM  ERRRUIL3-READ-SEC.
     IF       ERRRUIL3-END  =  "END"
              MOVE  "R"   TO  EDIT-OPTION OF DSP-KANRNO
              IF    ERR-FLG   = 0
                    MOVE   7     TO   ERR-FLG
                    MOVE  "C"    TO EDIT-CURSOR OF DSP-KANRNO
              END-IF
              GO               TO     PARA-CHK2-EXIT
     END-IF.
*
     MOVE     "   "            TO     ERR1-FLG
                                      ERR2-FLG
                                      ERR3-FLG
                                      ERR4-FLG.
 PARA-CHK2-04.
     IF       ERR3-F01    =   DSP-KANRNO
              MOVE ERR3-F02        TO     LINK-OUT-BDATE
              MOVE ERR3-F03        TO     LINK-OUT-BTIME
              IF   ERR3-F043  =   "01" OR "02"
                   MOVE      "HIT"      TO    ERR1-FLG
              END-IF
              IF   ERR3-F043  =   "03"
                   MOVE      "HIT"      TO    ERR2-FLG
              END-IF
              IF   ERR3-F043  =   "04"
                   MOVE      "HIT"      TO    ERR3-FLG
              END-IF
              IF   ERR3-F043  =   "05"
                   MOVE      "HIT"      TO    ERR4-FLG
              END-IF
     END-IF.
     PERFORM  ERRRUIL3-READ-SEC.
     IF     ( ERRRUIL3-END  =  "END"       )  OR
            ( ERR3-F01  NOT =  DSP-KANRNO )
              GO              TO        PARA-CHK2-05
     ELSE
              GO              TO        PARA-CHK2-04
     END-IF.
*受注売上
 PARA-CHK2-05.
     IF       DSP-ERR1   =   "Y"
         IF   ERR1-FLG   =   "HIT"
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR1
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR1
         ELSE
              MOVE  "R"   TO  EDIT-OPTION OF DSP-ERR1
         END-IF
     ELSE
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR1
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR1
     END-IF.
*入荷実績
 PARA-CHK2-06.
     IF       DSP-ERR2   =   "Y"
         IF   ERR2-FLG   =   "HIT"
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR2
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR2
         ELSE
              MOVE  "R"   TO  EDIT-OPTION OF DSP-ERR2
         END-IF
     ELSE
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR2
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR2
     END-IF.
*移動依頼
 PARA-CHK2-07.
     IF       DSP-ERR3   =   "Y"
         IF   ERR3-FLG   =   "HIT"
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR3
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR3
         ELSE
              MOVE  "R"   TO  EDIT-OPTION OF DSP-ERR3
         END-IF
     ELSE
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR3
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR3
     END-IF.
*作業実績
 PARA-CHK2-08.
     IF       DSP-ERR4   =   "Y"
         IF   ERR4-FLG   =   "HIT"
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR4
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR4
         ELSE
              MOVE  "R"   TO  EDIT-OPTION OF DSP-ERR4
         END-IF
     ELSE
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR4
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR4
     END-IF.
*総合判定
 PARA-CHK2-09.
     IF    (( DSP-ERR1  =  "Y" ) AND ( ERR1-FLG  NOT =  "HIT" ))
        OR (( DSP-ERR2  =  "Y" ) AND ( ERR2-FLG  NOT =  "HIT" ))
        OR (( DSP-ERR3  =  "Y" ) AND ( ERR3-FLG  NOT =  "HIT" ))
        OR (( DSP-ERR4  =  "Y" ) AND ( ERR4-FLG  NOT =  "HIT" ))
              IF    ERR-FLG   = 0
                    MOVE      7    TO  ERR-FLG
                    MOVE     "R"   TO  EDIT-OPTION OF DSP-KANRNO
                    MOVE     "C"   TO  EDIT-CURSOR OF DSP-KANRNO
              END-IF
              GO                 TO   PARA-CHK2-EXIT
     END-IF.
*
 PARA-CHK2-10.
     MOVE    "4"       TO    PSW.
*
 PARA-CHK2-EXIT.
     EXIT.
****************************************************************
*　　エラー累積ファイル３　順ＲＥＡＤ
****************************************************************
 ERRRUIL3-READ-SEC        SECTION.
*
     MOVE   "ERRRUIL3-READ-SEC"  TO   S-NAME.
*
 ERRRUIL3-READ-001.
     READ  ERRRUIL3
           AT  END     MOVE  "END"  TO  ERRRUIL3-END
                       GO           TO  ERRRUIL3-READ-EXIT
     END-READ.
*処理件数カウント／画面表示
     ADD   1                    TO   RD-CNT.
*    IF  RD-CNT(6:3)  =  "000" OR "500"
*        DISPLAY "READ-CNT = " RD-CNT UPON  CONS
*    END-IF.
*
 ERRRUIL3-READ-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 3 )                2.1       *
****************************************************************
 DSP-PARA3-SEC         SECTION.
     MOVE     "DSP-PARA3-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK3-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*戻り
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*               PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-PARA3-SEC
     END-EVALUATE.
*
 DSP-PARA3-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック(PSW=3)                2.1.1     *
****************************************************************
 PARA-CHK3-SEC             SECTION.
     MOVE     "PARA-CHK3-SEC"     TO   S-NAME.
*
 PARA-CHK3-01.
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       DSP-BTDT  NOT NUMERIC
         OR   DSP-BTDT  =  ZERO
              MOVE   9       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDT
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDT
              GO             TO   PARA-CHK3-EXIT
     ELSE
***           バッチ_（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-BTDT       TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  4        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDT
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDT
                   GO             TO   PARA-CHK3-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-BTDT
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-BTDT
     END-IF.
*
*バッチ_（時間）チェック
***  バッチ_（時間）未入力チェック
     IF       DSP-BTTM  NOT NUMERIC
         OR   DSP-BTTM  =   ZERO
                   MOVE   9       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTM
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTM
                   GO             TO   PARA-CHK3-EXIT
     ELSE
***           バッチ_（時間）論理チェック
              MOVE     DSP-BTTM    TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-FLG  =  ZERO
                              MOVE    5    TO    ERR-FLG
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF DSP-BTTM
                       MOVE "C"    TO EDIT-CURSOR OF DSP-BTTM
                       GO          TO PARA-CHK3-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF DSP-BTTM
                       MOVE SPACE  TO EDIT-CURSOR OF DSP-BTTM
              END-IF
     END-IF.
*
 PARA-CHK3-02.
*累積ファイル　存在チェック（ＡＬＬ" " )
     IF     ( DSP-ERR1 =  " " ) AND
            ( DSP-ERR2 =  " " ) AND
            ( DSP-ERR3 =  " " ) AND
            ( DSP-ERR4 =  " " )
              MOVE     DSP-BTDT         TO     ERR4-F02
              MOVE     DSP-BTTM         TO     ERR4-F03
              MOVE     "01"             TO     ERR4-F043
              START    ERRRUIL4 KEY IS  >=     ERR4-F02 ERR4-F03
                                               ERR4-F043
                  INVALID
                       MOVE  "R"   TO  EDIT-OPTION OF DSP-BTDT
                       MOVE  "R"   TO  EDIT-OPTION OF DSP-BTTM
                       IF    ERR-FLG     = 0
                             MOVE   7     TO   ERR-FLG
                             MOVE  "C"
                                    TO EDIT-CURSOR OF DSP-BTDT
                       END-IF
                       GO               TO     PARA-CHK3-EXIT
              END-START
              PERFORM  ERRRUIL4-READ-SEC
              IF       ERRRUIL4-END  =  "END"
                       MOVE  "R"   TO  EDIT-OPTION OF DSP-BTDT
                       MOVE  "R"   TO  EDIT-OPTION OF DSP-BTTM
                       IF    ERR-FLG   = 0
                             MOVE   7     TO   ERR-FLG
                             MOVE  "C"
                                    TO EDIT-CURSOR OF DSP-BTDT
                       END-IF
                       GO               TO     PARA-CHK3-EXIT
              END-IF
              IF     ( ERR4-F02    =   DSP-BTDT ) AND
                     ( ERR4-F03    =   DSP-BTTM ) AND
                     (( ERR4-F043  =   "01" ) OR
                      ( ERR4-F043  =   "02" ) OR
                      ( ERR4-F043  =   "03" ) OR
                      ( ERR4-F043  =   "04" ) OR
                      ( ERR4-F043  =   "05" ))
                       MOVE  "M"   TO  EDIT-OPTION OF DSP-BTDT
                       MOVE  "M"   TO  EDIT-OPTION OF DSP-BTTM
                       MOVE  " "   TO  EDIT-CURSOR OF DSP-BTDT
                       MOVE  " "   TO  EDIT-CURSOR OF DSP-BTTM
                       MOVE ERR4-F01   TO     LINK-OUT-KANRINO
                       GO          TO  PARA-CHK3-10
              ELSE
                       MOVE   7     TO  ERR-FLG
                       MOVE  "R"    TO  EDIT-OPTION OF DSP-BTDT
                       MOVE  "R"    TO  EDIT-OPTION OF DSP-BTTM
                       MOVE  "C"    TO  EDIT-CURSOR OF DSP-BTDT
                       GO           TO  PARA-CHK3-EXIT
              END-IF
     END-IF.
 PARA-CHK3-03.
*累積ファイル　存在チェック
     MOVE     DSP-BTDT         TO     ERR4-F02.
     MOVE     DSP-BTTM         TO     ERR4-F03.
     MOVE     "01"             TO     ERR4-F043.
     START    ERRRUIL4 KEY IS  >=     ERR4-F02 ERR4-F03
                                      ERR4-F043
         INVALID
              MOVE  "R"   TO  EDIT-OPTION OF DSP-BTDT
              MOVE  "R"   TO  EDIT-OPTION OF DSP-BTTM
              IF    ERR-FLG   = 0
                    MOVE   7     TO   ERR-FLG
                    MOVE  "C"    TO   EDIT-CURSOR OF DSP-BTDT
              END-IF
              GO                 TO   PARA-CHK3-EXIT
     END-START.
     PERFORM  ERRRUIL4-READ-SEC.
     IF       ERRRUIL4-END  =  "END"
              MOVE  "R"   TO  EDIT-OPTION OF DSP-BTDT
              MOVE  "R"   TO  EDIT-OPTION OF DSP-BTTM
              IF    ERR-FLG   = 0
                    MOVE   7     TO   ERR-FLG
                    MOVE  "C"    TO EDIT-CURSOR OF DSP-BTDT
              END-IF
              GO               TO     PARA-CHK3-EXIT
     END-IF.
*
     MOVE     "   "            TO     ERR1-FLG
                                      ERR2-FLG
                                      ERR3-FLG
                                      ERR4-FLG.
 PARA-CHK3-04.
*T
*    DISPLAY "ERR-F01=" ERR4-F01 UPON CONS.
*T
     IF     ( ERR4-F02    =   DSP-BTDT )  AND
            ( ERR4-F03    =   DSP-BTTM )
              MOVE ERR4-F01   TO     LINK-OUT-KANRINO
              IF   ERR4-F043  =   "01" OR "02"
                   MOVE      "HIT"      TO    ERR1-FLG
              END-IF
              IF   ERR4-F043  =   "03"
                   MOVE      "HIT"      TO    ERR2-FLG
              END-IF
              IF   ERR4-F043  =   "04"
                   MOVE      "HIT"      TO    ERR3-FLG
              END-IF
              IF   ERR4-F043  =   "05"
                   MOVE      "HIT"      TO    ERR4-FLG
              END-IF
     END-IF.
     PERFORM  ERRRUIL4-READ-SEC.
     IF     ( ERRRUIL4-END  =  "END"       )  OR
            ( ERR4-F02  NOT =  DSP-BTDT    )  OR
            ( ERR4-F03  NOT =  DSP-BTTM    )
              GO              TO        PARA-CHK3-05
     ELSE
              GO              TO        PARA-CHK3-04
     END-IF.
*受注売上
 PARA-CHK3-05.
     IF       DSP-ERR1   =   "Y"
         IF   ERR1-FLG   =   "HIT"
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR1
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR1
         ELSE
              MOVE  "R"   TO  EDIT-OPTION OF DSP-ERR1
         END-IF
     ELSE
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR1
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR1
     END-IF.
*入荷実績
 PARA-CHK3-06.
     IF       DSP-ERR2   =   "Y"
         IF   ERR2-FLG   =   "HIT"
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR2
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR2
         ELSE
              MOVE  "R"   TO  EDIT-OPTION OF DSP-ERR2
         END-IF
     ELSE
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR2
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR2
     END-IF.
*移動依頼
 PARA-CHK3-07.
     IF       DSP-ERR3   =   "Y"
         IF   ERR3-FLG   =   "HIT"
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR3
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR3
         ELSE
              MOVE  "R"   TO  EDIT-OPTION OF DSP-ERR3
         END-IF
     ELSE
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR3
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR3
     END-IF.
*作業実績
 PARA-CHK3-08.
     IF       DSP-ERR4   =   "Y"
         IF   ERR4-FLG   =   "HIT"
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR4
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR4
         ELSE
              MOVE  "R"   TO  EDIT-OPTION OF DSP-ERR4
         END-IF
     ELSE
              MOVE  "M"   TO  EDIT-OPTION OF DSP-ERR4
              MOVE  " "   TO  EDIT-CURSOR OF DSP-ERR4
     END-IF.
*総合判定
 PARA-CHK3-09.
     IF    (( DSP-ERR1  =  "Y" ) AND ( ERR1-FLG  NOT =  "HIT" ))
        OR (( DSP-ERR2  =  "Y" ) AND ( ERR2-FLG  NOT =  "HIT" ))
        OR (( DSP-ERR3  =  "Y" ) AND ( ERR3-FLG  NOT =  "HIT" ))
        OR (( DSP-ERR4  =  "Y" ) AND ( ERR4-FLG  NOT =  "HIT" ))
              IF    ERR-FLG   = 0
                    MOVE      7    TO  ERR-FLG
                    MOVE     "R"   TO  EDIT-OPTION OF DSP-BTDT
                    MOVE     "R"   TO  EDIT-OPTION OF DSP-BTTM
                    MOVE     "C"   TO  EDIT-CURSOR OF DSP-BTDT
              END-IF
              GO                 TO   PARA-CHK3-EXIT
     END-IF.
*
 PARA-CHK3-10.
     MOVE    "4"       TO    PSW.
*
 PARA-CHK3-EXIT.
     EXIT.
****************************************************************
*　　エラー累積ファイル４　順ＲＥＡＤ
****************************************************************
 ERRRUIL4-READ-SEC        SECTION.
*
     MOVE   "ERRRUIL4-READ-SEC"  TO   S-NAME.
*
 ERRRUIL4-READ-001.
     READ  ERRRUIL4
           AT  END     MOVE  "END"  TO  ERRRUIL4-END
                       GO           TO  ERRRUIL4-READ-EXIT
     END-READ.
*処理件数カウント／画面表示
     ADD   1                    TO   RD-CNT.
*    IF  RD-CNT(6:3)  =  "000" OR "500"
*        DISPLAY "READ-CNT = " RD-CNT UPON  CONS
*    END-IF.
*
 ERRRUIL4-READ-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 4 )                2.1       *
****************************************************************
 DSP-PARA4-SEC         SECTION.
     MOVE     "DSP-PARA4-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK4-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*戻り
         WHEN   "F006"
                IF   DSP-SENTAK  =  " "
                     MOVE    "2"      TO   PSW
                ELSE
                     MOVE    "3"      TO   PSW
                END-IF
*               PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-PARA4-SEC
     END-EVALUATE.
*
 DSP-PARA4-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック(PSW=4)                2.1.1     *
****************************************************************
 PARA-CHK4-SEC             SECTION.
     MOVE     "PARA-CHK4-SEC"     TO   S-NAME.
*
*出力区分チェック
     IF    (  DSP-SYUKBN NOT =    " "  )  AND
           (  DSP-SYUKBN NOT =    "1"  )  AND
           (  DSP-SYUKBN NOT =    "2"  )
              MOVE      8    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SYUKBN
              GO             TO   PARA-CHK4-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-SYUKBN
     END-IF.
*
     MOVE    "K"       TO    PSW.
*
 PARA-CHK4-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-KAKU-SEC         SECTION.
     MOVE     "DSP-KAKU-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                 IF     ( DSP-ERR1 =  " " ) AND
                        ( DSP-ERR2 =  " " ) AND
                        ( DSP-ERR3 =  " " ) AND
                        ( DSP-ERR4 =  " " )
                          MOVE   "1"    TO     LINK-OUT-ERR1
                                               LINK-OUT-ERR2
                                               LINK-OUT-ERR3
                                               LINK-OUT-ERR4
                 END-IF
                 IF     DSP-ERR1 =  "Y"
                        MOVE   "1"    TO   LINK-OUT-ERR1
                 END-IF
                 IF     DSP-ERR2 =  "Y"
                        MOVE   "1"    TO   LINK-OUT-ERR2
                 END-IF
                 IF     DSP-ERR3 =  "Y"
                        MOVE   "1"    TO   LINK-OUT-ERR3
                 END-IF
                 IF     DSP-ERR4 =  "Y"
                        MOVE   "1"    TO   LINK-OUT-ERR4
                 END-IF
                 MOVE   DSP-SENTAK       TO   LINK-OUT-SEL
                 IF     DSP-SENTAK  =  " "
                        MOVE DSP-KANRNO   TO LINK-OUT-KANRINO
                 ELSE
                        MOVE DSP-BTDT      TO LINK-OUT-BDATE
                        MOVE DSP-BTTM      TO LINK-OUT-BTIME
                 END-IF
                 MOVE   DSP-SYUKBN    TO   LINK-OUT-OUTPUT
*T
*        DISPLAY "LINK-OUT-ERR1   = " LINK-OUT-ERR1    UPON CONS
*        DISPLAY "LINK-OUT-ERR2   = " LINK-OUT-ERR2    UPON CONS
*        DISPLAY "LINK-OUT-ERR3   = " LINK-OUT-ERR3    UPON CONS
*        DISPLAY "LINK-OUT-ERR4   = " LINK-OUT-ERR4    UPON CONS
*        DISPLAY "LINK-OUT-SEL    = " LINK-OUT-SEL     UPON CONS
*        DISPLAY "LINK-OUT-KANRINO= " LINK-OUT-KANRINO UPON CONS
*        DISPLAY "LINK-OUT-BDATE  = " LINK-OUT-BDATE   UPON CONS
*        DISPLAY "LINK-OUT-BTIME  = " LINK-OUT-BTIME   UPON CONS
*        DISPLAY "LINK-OUT-OUTPUT = " LINK-OUT-OUTPUT  UPON CONS
*T
                MOVE    "END"         TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "4"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
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
           MOVE    SPACE              TO   DSP-MSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-GUIDE
***      パラメタ項目
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
***      パラメタ項目
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
***      パラメタ項目
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
***      確認
         WHEN   "K"
                MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-GUIDE
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FER01101"          TO   DSP-FMT.
     WRITE    DSP-FER01101.
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
*パラメタ項目
         WHEN   "1"
                MOVE    "BODY01"  TO   DSP-GRP
         WHEN   "2"
                MOVE    "BODY02"  TO   DSP-GRP
         WHEN   "3"
                MOVE    "BODY03"  TO   DSP-GRP
         WHEN   "4"
                MOVE    "BODY04"  TO   DSP-GRP
*確認
         WHEN   "K"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FER01101"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FER01101.
*プログラムＩＤ転送
     MOVE    WORK-PGID            TO   DSP-PGID.
*ＦＯＲＭＩＤ転送
     MOVE    WORK-FORMID          TO   DSP-FRID.
*システム日付転送
     MOVE    SYS-DATE(1:4)        TO   DSP-SDATE(1:4).
     MOVE    "/"                  TO   DSP-SDATE(5:1).
     MOVE    SYS-DATE(5:2)        TO   DSP-SDATE(6:2).
     MOVE    "/"                  TO   DSP-SDATE(8:1).
     MOVE    SYS-DATE(7:2)        TO   DSP-SDATE(9:2).
*システム時間転送
     MOVE    WK-TIME(1:2)         TO   DSP-STIME(1:2).
     MOVE    ":"                  TO   DSP-STIME(3:1).
     MOVE    WK-TIME(3:2)         TO   DSP-STIME(4:2).
     MOVE    ":"                  TO   DSP-STIME(6:1).
     MOVE    WK-TIME(5:2)         TO   DSP-STIME(7:2).
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  リカバリ対象
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-ERR1.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-ERR1.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-ERR2.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-ERR2.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-ERR3.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-ERR3.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-ERR4.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-ERR4.
***  選択
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SENTAK.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SENTAK.
***  管理番号
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KANRNO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KANRNO.
***  バッチ_（日付）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTDT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTDT.
***  バッチ_（時間）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTM.
***  出力区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYUKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYUKBN.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             HTANMS  ERRRUIL3  ERRRUIL4  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SER0110I   END PROGRAM  >>******************
