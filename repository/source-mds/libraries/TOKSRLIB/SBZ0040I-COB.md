# SBZ0040I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBZ0040I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　部門間在庫移動機能構築　　　      *
*    モジュール名　　　　：　部門間在庫移動リスト発行指示　　  *
*    作成日／更新日　　　：　2018/01/22                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　部門間在庫移動リスト発行の発行指　*
*                        ：　示を行なう。　　　　　　　　　　  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBZ0040I.
 AUTHOR.               NAV-ASSIST.
 DATE-WRITTEN.         2018/01/22.
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
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FBZ00401  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SBZ0040I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FBZ00401".
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SOKKBN-FLG               PIC  9(01)  VALUE  ZERO.
*ワーク領域
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
             VALUE NC"発行日付区分を入力してください".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"発行日付区分は、１，２，３です。".
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
             VALUE NC"正しい発行区分を入力して下さい。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい出力区分を入力して下さい".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS   7  PIC   N(20).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
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
 01  LINK-OUT-KBN          PIC X(01).
 01  LINK-OUT-STDATE       PIC 9(08).
 01  LINK-OUT-EDDATE       PIC 9(08).
 01  LINK-OUT-HAKKBN       PIC X(01).
 01  LINK-OUT-SYUKBN       PIC X(01).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-OUT-KBN
                                     LINK-OUT-STDATE
                                     LINK-OUT-EDDATE
                                     LINK-OUT-HAKKBN
                                     LINK-OUT-SYUKBN.
**************************************************************
 DECLARATIVES.
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
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
*ワークの初期化
     INITIALIZE         FLG-AREA
                        LINK-OUT-KBN
                        LINK-OUT-STDATE
                        LINK-OUT-EDDATE
                        LINK-OUT-HAKKBN
                        LINK-OUT-SYUKBN.
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
*範囲入力１
         WHEN      "1"  PERFORM   DSP-HANI1-SEC
*範囲入力２
         WHEN      "2"  PERFORM   DSP-HANI2-SEC
*確認入力
         WHEN      "3"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    範囲指定入力（発行日付区分）
****************************************************************
 DSP-HANI1-SEC          SECTION.
     MOVE      "DSP-HANI1-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HANI1-CHK-SEC
*終了
         WHEN   "F005"
                MOVE     "END"    TO   END-FLG
                MOVE     "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE     "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE      5       TO   ERR-FLG
                GO                TO   DSP-HANI1-EXIT
     END-EVALUATE.
*
 DSP-HANI1-EXIT.
     EXIT.
****************************************************************
*    範囲指定　入力項目チェック
****************************************************************
 HANI1-CHK-SEC              SECTION.
     MOVE    "HANI1-CHK-SEC"       TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*発行日付区分
     IF       DSP-TAIKBN  =  "1"  OR  "2"  OR  "3"
              EVALUATE  DSP-TAIKBN
                  WHEN  "1"  MOVE NC"作成日"  TO  DSP-TAINM
                  WHEN  "2"  MOVE NC"計上日"  TO  DSP-TAINM
                  WHEN  "3"  MOVE NC"修正日"  TO  DSP-TAINM
                  WHEN OTHER MOVE NC"＊＊＊"  TO  DSP-TAINM
              END-EVALUATE
     ELSE
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TAIKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TAIKBN
              MOVE NC"＊＊＊" TO  DSP-TAINM
              GO             TO   HANI1-CHK-EXIT
     END-IF.
*
     MOVE    "2"       TO    PSW.
*
 HANI1-CHK-EXIT.
     EXIT.
****************************************************************
*    範囲指定入力（発行日付区分）
****************************************************************
 DSP-HANI2-SEC          SECTION.
     MOVE      "DSP-HANI2-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HANI2-CHK-SEC
*終了
         WHEN   "F005"
                MOVE     "END"    TO   END-FLG
                MOVE     "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE     "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*項目戻し
         WHEN   "F006"
                MOVE     "1"      TO   PSW
         WHEN   OTHER
                MOVE      5       TO   ERR-FLG
                GO                TO   DSP-HANI2-EXIT
     END-EVALUATE.
*
 DSP-HANI2-EXIT.
     EXIT.
****************************************************************
*    範囲指定　入力項目チェック
****************************************************************
 HANI2-CHK-SEC              SECTION.
     MOVE    "HANI2-CHK-SEC"       TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*日付範囲（開始日付）
     IF ( DSP-TAIDTS  =  SPACE    ) OR
        ( DSP-TAIDTS  =  ZERO     ) OR
        ( DSP-TAIDTS  =  99999999 )
          MOVE   ZERO       TO       DSP-TAIDTS
     ELSE
          MOVE  "2"         TO       LINK-IN-KBN
          MOVE   DSP-TAIDTS TO       LINK-IN-YMD8
          CALL  "SKYDTCKB"  USING    LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8
          IF     LINK-OUT-RET NOT =  ZERO
                 MOVE   3    TO   ERR-FLG
                 MOVE  "C"   TO   EDIT-CURSOR OF DSP-TAIDTS
                 MOVE  "R"   TO   EDIT-OPTION OF DSP-TAIDTS
                 GO          TO   HANI2-CHK-EXIT
         END-IF
     END-IF.
*日付範囲（終了日付）
     IF ( DSP-TAIDTE  =  SPACE    ) OR
        ( DSP-TAIDTE  =  ZERO     ) OR
        ( DSP-TAIDTE  =  99999999 )
          MOVE   99999999   TO       DSP-TAIDTE
     ELSE
          MOVE  "2"         TO       LINK-IN-KBN
          MOVE   DSP-TAIDTE TO       LINK-IN-YMD8
          CALL  "SKYDTCKB"  USING    LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8
          IF     LINK-OUT-RET NOT =  ZERO
                 MOVE   3    TO   ERR-FLG
                 MOVE  "C"   TO   EDIT-CURSOR OF DSP-TAIDTE
                 MOVE  "R"   TO   EDIT-OPTION OF DSP-TAIDTE
                 GO          TO   HANI2-CHK-EXIT
          END-IF
     END-IF
*    日付大小チェック
     IF   DSP-TAIDTS  >   DSP-TAIDTE
          MOVE      4    TO   ERR-FLG
          MOVE     "R"   TO   EDIT-OPTION  OF  DSP-TAIDTS
          MOVE     "R"   TO   EDIT-OPTION  OF  DSP-TAIDTE
          MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-TAIDTS
          GO             TO   HANI2-CHK-EXIT
     END-IF.
*発行区分チェック
     IF       DSP-HAKKBN    =  " "  OR  "1"
              CONTINUE
     ELSE
              MOVE   6       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HAKKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HAKKBN
              GO             TO   HANI2-CHK-EXIT
     END-IF.
*出力区分チェック
     IF       DSP-SYUKBN    =  " "  OR  "1"
              CONTINUE
     ELSE
              MOVE   7       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SYUKBN
              GO             TO   HANI2-CHK-EXIT
     END-IF.
*
     MOVE    "3"       TO    PSW.
*
 HANI2-CHK-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 6 )                2.2       *
****************************************************************
 DSP-KAKU-SEC         SECTION.
     MOVE      "DSP-KAKU-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    DSP-TAIKBN    TO   LINK-OUT-KBN
                MOVE    DSP-TAIDTS    TO   LINK-OUT-STDATE
                MOVE    DSP-TAIDTE    TO   LINK-OUT-EDDATE
                MOVE    DSP-HAKKBN    TO   LINK-OUT-HAKKBN
                MOVE    DSP-SYUKBN    TO   LINK-OUT-SYUKBN
                MOVE    "END"         TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM  INIT-DSP-SEC
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
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
           MOVE    SPACE              TO   DSP-MSGSPC
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSGSPC
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FBZ00401"          TO   DSP-FMT.
     WRITE    DSP-FBZ00401.
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
*範囲指定１
         WHEN   "1"
                MOVE    "HEAD01"  TO   DSP-GRP
*範囲指定２
         WHEN   "2"
                MOVE    "BODY01"  TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE   "FBZ00401"            TO   DSP-FMT.
     READ    DSPFILE.
*
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
     MOVE    SPACE                TO   DSP-FBZ00401.
*プログラムＩＤ転送
     MOVE    WORK-PGID            TO   DSP-PGID.
*ＦＯＲＭＩＤ転送
     MOVE    WORK-FORMID          TO   DSP-FORMID.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INIT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  発行日付区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TAIKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TAIKBN.
***  日付開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TAIDTS.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TAIDTS.
***  日付終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TAIDTE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TAIDTE.
***  発行区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HAKKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HAKKBN.
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
     CLOSE             DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SBZ0040I   END PROGRAM  >>******************

```
