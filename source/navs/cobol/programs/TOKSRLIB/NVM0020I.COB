****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　マスタ関係　        *
*    モジュール名　　　　：　Ｄ３６５送受信グループマスタ保守　*
*    作成日／作成者　　　：　2019/12/27 NAV HASHIMOTO          *
*    処理概要　　　　　　：　Ｄ３６５送受信グループマスタの　　*
*                          　登録、修正、削除を行なう。　　　　*
*                          　　　　　　　　　　　　　　　　　　*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVM0020I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2019/12/27.
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
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPFILE
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
*Ｄ３６５送受信データ種別マスタ
     SELECT  DNDATSF   ASSIGN    TO        DNDATSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       DND-F01
                       FILE      STATUS    DND-ST.
*
*Ｄ３６５送受信グループマスタ
     SELECT  DNGROPF   ASSIGN    TO        DNGROPL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       DNG-F01
                       FILE      STATUS    DNG-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FVM00201  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
*    FILE = Ｄ３６５送受信データ種別マスタ             *
****************************************************************
 FD  DNDATSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      DNDATSF   OF   XFDLIB
                       JOINING   DND       AS   PREFIX.
*
****************************************************************
*    FILE = Ｄ３６５送受信グループマスタ             *
****************************************************************
 FD  DNGROPF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      DNGROPF   OF   XFDLIB
                       JOINING   DNG       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DND-ST                   PIC  X(02).
     03  DNG-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SAKKBN-FLG               PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SYORI                PIC  9(01)  VALUE  ZERO.
*
     03  WRK-DSYU                 PIC  X(02)  VALUE  SPACE.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)  VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC  9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC  9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC  9(02)  VALUE  ZERO.
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
     03  PF-MSG-R   OCCURS   2    PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER               PIC   N(25)
             VALUE NC"処理区分を正しく入力してください。".
     03  ERR-MSG2.
         05  FILLER               PIC   N(25)
             VALUE NC"既に登録済のデータです".
     03  ERR-MSG3.
         05  FILLER               PIC   N(25)
             VALUE NC"対象データが存在しません。".
     03  ERR-MSG4.
         05  FILLER               PIC   N(25)
             VALUE NC"必須項目です。必ず入力して下さい。".
     03  ERR-MSG5.
         05  FILLER               PIC   N(25)
         VALUE NC"Ｄ３６５送受信データ種別マスタに未登録です。".
     03  ERR-MSG6.
         05  FILLER               PIC   N(25)
             VALUE NC"無効キーです".
     03  ERR-MSG7.
         05  FILLER               PIC   N(25)
             VALUE NC"データ種別が重複しています。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  7    PIC   N(25).
*
 01  FILE-ERR.
     03  DND-ERR           PIC N(20) VALUE
                        NC"送受信データ種別マスタエラー".
     03  DNG-ERR           PIC N(20) VALUE
                        NC"送受信グループマスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*画面日付／時刻表示編集
 01  GAMEN-HIDUKE.
     03  GAMEN-YYYY        PIC  9(04).
     03  FILLER            PIC  X(01)  VALUE  "/".
     03  GAMEN-MM          PIC  9(02).
     03  FILLER            PIC  X(01)  VALUE  "/".
     03  GAMEN-DD          PIC  9(02).
 01  GAMEN-TIME.
     03  GAMEN-HH          PIC  9(02).
     03  FILLER            PIC  X(01)  VALUE  ":".
     03  GAMEN-FF          PIC  9(02).
     03  FILLER            PIC  X(01)  VALUE  ":".
     03  GAMEN-BB          PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC  X(01).
 01  LINK-IN-YMD6          PIC  9(06).
 01  LINK-IN-YMD8          PIC  9(08).
 01  LINK-OUT-RET          PIC  X(01).
 01  LINK-OUT-YMD          PIC  9(08).
*
****************************************************************
 LINKAGE               SECTION.
****************************************************************
 01  PAR-BUMON             PIC  X(04).
 01  PAR-TANCD             PIC  X(02).
*
**************************************************************
 PROCEDURE             DIVISION   USING  PAR-BUMON  PAR-TANCD.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DND-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DNDATSF.
     DISPLAY     DND-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DND-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DNG-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DNGROPF.
     DISPLAY     DNG-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DNG-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.

     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.

     STOP    RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN     I-O       DNGROPF   DSPFILE.
     OPEN     INPUT     DNDATSF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             システム日付取得
****************************************************************
 SDATE-GET-SEC              SECTION.
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
*画面表示日付／時刻編集
     MOVE      SYS-DATE(1:4)      TO   GAMEN-YYYY.
     MOVE      SYS-DATE(5:2)      TO   GAMEN-MM.
     MOVE      SYS-DATE(7:2)      TO   GAMEN-DD.
     MOVE      WK-TIME(1:2)       TO   GAMEN-HH.
     MOVE      WK-TIME(3:2)       TO   GAMEN-FF.
     MOVE      WK-TIME(5:2)       TO   GAMEN-BB.
*
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*処理区分入力
         WHEN      "1"  PERFORM   DSP-HEAD1-SEC
*キー項目入力
         WHEN      "2"  PERFORM   DSP-HEAD2-SEC
*明細項目入力
         WHEN      "3"  PERFORM   DSP-BODY-SEC
*確認入力
         WHEN      "4"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-HEAD1-SEC         SECTION.
     MOVE     "DSP-HEAD1-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD1-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-HEAD1-SEC
     END-EVALUATE.
*
 DSP-HEAD1-EXIT.
     EXIT.
****************************************************************
*             処理区分チェック                       2.1.1     *
****************************************************************
 HEAD1-CHK-SEC             SECTION.
*処理区分 未入力はエラー
     IF     DSP-SYORI NOT NUMERIC
         OR DSP-SYORI = ZERO
         MOVE  1                 TO  ERR-FLG
         MOVE  SPACE             TO  DSP-SYORI (1:1)
         MOVE  "R"               TO  EDIT-OPTION OF DSP-SYORI
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-SYORI
         GO TO  HEAD1-CHK-EXIT
     END-IF.
*処理区分＝１，２，３以外はエラー
     IF    DSP-SYORI    =   1   OR   2   OR   3
              MOVE     "2"       TO   PSW
*             同一モードでループさせるため
              MOVE   DSP-SYORI   TO  SAV-SYORI
              MOVE  "M"          TO  EDIT-OPTION OF DSP-SYORI
              MOVE  SPACE        TO  EDIT-CURSOR OF DSP-SYORI
     ELSE
              MOVE   1           TO  ERR-FLG
              MOVE  "R"          TO  EDIT-OPTION OF DSP-SYORI
              MOVE  "C"          TO  EDIT-CURSOR OF DSP-SYORI
              GO TO  HEAD1-CHK-EXIT
     END-IF.
*
 HEAD1-CHK-EXIT.
     EXIT.
****************************************************************
*             キー項目入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-HEAD2-SEC         SECTION.
     MOVE     "DSP-HEAD2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD2-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-HEAD2-SEC
     END-EVALUATE.
*
 DSP-HEAD2-EXIT.
     EXIT.
****************************************************************
*             キー項目チェック                       2.2.1     *
****************************************************************
 HEAD2-CHK-SEC         SECTION.
     MOVE  "HEAD2-CHK-SEC"       TO  S-NAME.
*キー項目 未入力チェック
***  送受信グループ_
     IF  DSP-SGRPNO  = SPACE
         MOVE   4           TO  ERR-FLG
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SGRPNO
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SGRPNO
         GO TO  HEAD2-CHK-EXIT
     ELSE
         MOVE  "M"          TO  EDIT-OPTION OF DSP-SGRPNO
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-SGRPNO
     END-IF.
*
*Ｄ３６５送受信グループマスタ ＲＥＡＤ
     PERFORM  DNGROPF-READ.
***  処理区分＝１（登録）
     IF  DSP-SYORI   =   1
***           データが存在すればエラー
         IF  READ-FLG   =   ZERO
             PERFORM   MOVE-DSP-SEC
             MOVE   2          TO  ERR-FLG
             MOVE  "R"         TO  EDIT-OPTION OF DSP-SGRPNO
             MOVE  "C"         TO  EDIT-CURSOR OF DSP-SGRPNO
             GO TO  HEAD2-CHK-EXIT
***           データが存在しなければＯＫ（ボディ入力へ）
         ELSE
             MOVE    "3"           TO   PSW
         END-IF
     ELSE
***  処理区分＝２（修正），３（削除）
***      データが存在すればＯＫ
         IF  READ-FLG   =   ZERO
             PERFORM   MOVE-DSP-SEC
***          処理区分＝２（修正）は，ボディ入力へ
             IF  DSP-SYORI  =  2
                 MOVE    "3"          TO   PSW
             ELSE
***              処理区分＝３（削除）は，確認入力へ
                 MOVE    "4"          TO   PSW
             END-IF
***         データが存在しなければエラー
         ELSE
                 MOVE     3            TO   ERR-FLG
                 GO  TO   HEAD2-CHK-EXIT
         END-IF
     END-IF.
*
 HEAD2-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 3 )              2.3       *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   BODY-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-BODY-SEC
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                       2.3.1     *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE   "BODY-CHK-SEC"       TO  S-NAME.
*
*送受信グループ名
     IF  DSP-GRPNAM    = SPACE
         MOVE   4           TO  ERR-FLG
         MOVE  "R"          TO  EDIT-OPTION OF DSP-GRPNAM
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-GRPNAM
         GO TO  BODY-CHK-EXIT
     ELSE
         MOVE  "M"          TO  EDIT-OPTION OF DSP-GRPNAM
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-GRPNAM
     END-IF.
*データ種別１
     IF  DSP-DSYU1   = SPACE
         MOVE   4           TO  ERR-FLG
         MOVE  "R"          TO  EDIT-OPTION OF DSP-DSYU1
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-DSYU1
         GO TO  BODY-CHK-EXIT
     ELSE
         MOVE   DSP-DSYU1   TO  WRK-DSYU
         PERFORM  DNDATSF-READ
         IF  READ-FLG  =  1
             MOVE   5       TO  ERR-FLG
             MOVE  SPACE    TO  DSP-DNAM1
             MOVE  "R"      TO  EDIT-OPTION OF DSP-DSYU1
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-DSYU1
             GO TO  BODY-CHK-EXIT
         ELSE
             MOVE  DND-F02  TO  DSP-DNAM1
             MOVE  "M"      TO  EDIT-OPTION OF DSP-DSYU1
             MOVE  SPACE    TO  EDIT-CURSOR OF DSP-DSYU1
         END-IF
     END-IF.
*データ種別２
     IF  DSP-DSYU2   = SPACE
         MOVE  SPACE        TO  DSP-DNAM2
         MOVE  "M"          TO  EDIT-OPTION OF DSP-DSYU2
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-DSYU2
*        GO TO  BODY-CHK-EXIT
     ELSE
         MOVE   DSP-DSYU2   TO  WRK-DSYU
         PERFORM  DNDATSF-READ
         IF  READ-FLG  =  1
             MOVE   5       TO  ERR-FLG
             MOVE  SPACE    TO  DSP-DNAM2
             MOVE  "R"      TO  EDIT-OPTION OF DSP-DSYU2
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-DSYU2
             GO TO  BODY-CHK-EXIT
         ELSE
             MOVE  DND-F02  TO  DSP-DNAM2
             MOVE  "M"      TO  EDIT-OPTION OF DSP-DSYU2
             MOVE  SPACE    TO  EDIT-CURSOR OF DSP-DSYU2
         END-IF
     END-IF.
*データ種別３
     IF  DSP-DSYU3   = SPACE
         MOVE  SPACE        TO  DSP-DNAM3
         MOVE  "M"          TO  EDIT-OPTION OF DSP-DSYU3
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-DSYU3
*        GO TO  BODY-CHK-EXIT
     ELSE
         MOVE   DSP-DSYU3   TO  WRK-DSYU
         PERFORM  DNDATSF-READ
         IF  READ-FLG  =  1
             MOVE   5       TO  ERR-FLG
             MOVE  SPACE    TO  DSP-DNAM3
             MOVE  "R"      TO  EDIT-OPTION OF DSP-DSYU3
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-DSYU3
             GO TO  BODY-CHK-EXIT
         ELSE
             MOVE  DND-F02  TO  DSP-DNAM3
             MOVE  "M"      TO  EDIT-OPTION OF DSP-DSYU3
             MOVE  SPACE    TO  EDIT-CURSOR OF DSP-DSYU3
         END-IF
     END-IF.
*データ種別４
     IF  DSP-DSYU4   = SPACE
         MOVE  SPACE        TO  DSP-DNAM4
         MOVE  "M"          TO  EDIT-OPTION OF DSP-DSYU4
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-DSYU4
*        GO TO  BODY-CHK-EXIT
     ELSE
         MOVE   DSP-DSYU4   TO  WRK-DSYU
         PERFORM  DNDATSF-READ
         IF  READ-FLG  =  1
             MOVE   5       TO  ERR-FLG
             MOVE  SPACE    TO  DSP-DNAM4
             MOVE  "R"      TO  EDIT-OPTION OF DSP-DSYU4
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-DSYU4
             GO TO  BODY-CHK-EXIT
         ELSE
             MOVE  DND-F02  TO  DSP-DNAM4
             MOVE  "M"      TO  EDIT-OPTION OF DSP-DSYU4
             MOVE  SPACE    TO  EDIT-CURSOR OF DSP-DSYU4
         END-IF
     END-IF.
*データ種別５
     IF  DSP-DSYU5   = SPACE
         MOVE  SPACE        TO  DSP-DNAM5
         MOVE  "M"          TO  EDIT-OPTION OF DSP-DSYU5
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-DSYU5
*        GO TO  BODY-CHK-EXIT
     ELSE
         MOVE   DSP-DSYU5   TO  WRK-DSYU
         PERFORM  DNDATSF-READ
         IF  READ-FLG  =  1
             MOVE   5       TO  ERR-FLG
             MOVE  SPACE    TO  DSP-DNAM5
             MOVE  "R"      TO  EDIT-OPTION OF DSP-DSYU5
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-DSYU5
             GO TO  BODY-CHK-EXIT
         ELSE
             MOVE  DND-F02  TO  DSP-DNAM5
             MOVE  "M"      TO  EDIT-OPTION OF DSP-DSYU5
             MOVE  SPACE    TO  EDIT-CURSOR OF DSP-DSYU5
         END-IF
     END-IF.
*
*    重複チェック
     IF  DSP-DSYU2 NOT = SPACE  OR
         DSP-DSYU3 NOT = SPACE  OR
         DSP-DSYU4 NOT = SPACE  OR
         DSP-DSYU5 NOT = SPACE
         IF  DSP-DSYU2  NOT =  SPACE
             IF  DSP-DSYU1  =  DSP-DSYU2
                 MOVE   7      TO  ERR-FLG
                 MOVE  "R"     TO  EDIT-OPTION OF DSP-DSYU2
                 MOVE  "C"     TO  EDIT-CURSOR OF DSP-DSYU2
                 GO TO  BODY-CHK-EXIT
             END-IF
             IF  DSP-DSYU2  =  DSP-DSYU3
                 MOVE   7      TO  ERR-FLG
                 MOVE  "R"     TO  EDIT-OPTION OF DSP-DSYU3
                 MOVE  "C"     TO  EDIT-CURSOR OF DSP-DSYU3
                 GO TO  BODY-CHK-EXIT
             END-IF
             IF  DSP-DSYU2  =  DSP-DSYU4
                 MOVE   7      TO  ERR-FLG
                 MOVE  "R"     TO  EDIT-OPTION OF DSP-DSYU4
                 MOVE  "C"     TO  EDIT-CURSOR OF DSP-DSYU4
                 GO TO  BODY-CHK-EXIT
             END-IF
             IF  DSP-DSYU2  =  DSP-DSYU5
                 MOVE   7      TO  ERR-FLG
                 MOVE  "R"     TO  EDIT-OPTION OF DSP-DSYU5
                 MOVE  "C"     TO  EDIT-CURSOR OF DSP-DSYU5
                 GO TO  BODY-CHK-EXIT
             END-IF
         END-IF
         IF  DSP-DSYU3  NOT =  SPACE
             IF  DSP-DSYU1  =  DSP-DSYU3
                 MOVE   7      TO  ERR-FLG
                 MOVE  "R"     TO  EDIT-OPTION OF DSP-DSYU3
                 MOVE  "C"     TO  EDIT-CURSOR OF DSP-DSYU3
                 GO TO  BODY-CHK-EXIT
             END-IF
             IF  DSP-DSYU3  =  DSP-DSYU4
                 MOVE   7      TO  ERR-FLG
                 MOVE  "R"     TO  EDIT-OPTION OF DSP-DSYU4
                 MOVE  "C"     TO  EDIT-CURSOR OF DSP-DSYU4
                 GO TO  BODY-CHK-EXIT
             END-IF
             IF  DSP-DSYU3  =  DSP-DSYU5
                 MOVE   7      TO  ERR-FLG
                 MOVE  "R"     TO  EDIT-OPTION OF DSP-DSYU5
                 MOVE  "C"     TO  EDIT-CURSOR OF DSP-DSYU5
                 GO TO  BODY-CHK-EXIT
             END-IF
         END-IF
         IF  DSP-DSYU4  NOT =  SPACE
             IF  DSP-DSYU1  =  DSP-DSYU4
                 MOVE   7      TO  ERR-FLG
                 MOVE  "R"     TO  EDIT-OPTION OF DSP-DSYU4
                 MOVE  "C"     TO  EDIT-CURSOR OF DSP-DSYU4
                 GO TO  BODY-CHK-EXIT
             END-IF
             IF  DSP-DSYU4  =  DSP-DSYU5
                 MOVE   7      TO  ERR-FLG
                 MOVE  "R"     TO  EDIT-OPTION OF DSP-DSYU5
                 MOVE  "C"     TO  EDIT-CURSOR OF DSP-DSYU5
                 GO TO  BODY-CHK-EXIT
             END-IF
         END-IF
         IF  DSP-DSYU5  NOT =  SPACE
             IF  DSP-DSYU1  =  DSP-DSYU5
                 MOVE   7      TO  ERR-FLG
                 MOVE  "R"     TO  EDIT-OPTION OF DSP-DSYU5
                 MOVE  "C"     TO  EDIT-CURSOR OF DSP-DSYU5
                 GO TO  BODY-CHK-EXIT
             END-IF
         END-IF
     END-IF.
     MOVE  "4"                   TO  PSW.
*
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 4 ）            2.4
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
                EVALUATE  DSP-SYORI
*-------------------登録
                    WHEN  "1"
                          PERFORM     FILE-WRT-SEC
*-------------------修正
                    WHEN  "2"
                          PERFORM     FILE-UPD-SEC
*-------------------削除
                    WHEN  "3"
                          PERFORM     FILE-DLT-SEC
                END-EVALUATE
                PERFORM   INIT-DSP-SEC
                MOVE    "2"      TO   PSW
                MOVE   SAV-SYORI TO   DSP-SYORI
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                IF  DSP-SYORI   =  1   OR   2
                    MOVE    "3"       TO   PSW
                ELSE
                    MOVE    "1"       TO   PSW
                END-IF
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
*    送受信データ種別マスタ更新　処理区分＝１（登録）2.4.1     *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE  "FILE-WRT-SEC"         TO  S-NAME.
*レコード初期クリア
     MOVE  SPACE                  TO  DNG-REC.
     INITIALIZE  DNG-REC.
*キー項目転送
*送受信データ種別
     MOVE  DSP-SGRPNO             TO  DNG-F01.
*明細項目
     PERFORM  MOVE-DNGROPF.
*ＥＤＩメッセージマスタ登録
     WRITE    DNG-REC.
*
 FILE-WRT-EXIT.
     EXIT.
****************************************************************
*    送受信グループマスタ更新　処理区分＝２（修正）2.4.2     *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE  "FILE-UPD-SEC"        TO  S-NAME.
*キー項目転送
*送受信データ種別Ｏ
     MOVE  DSP-SGRPNO            TO  DNG-F01.
*ＥＤＩメッセージマスタ ＲＥＡＤ（存在すればレコード更新）
     PERFORM  DNGROPF-READ.
     IF  READ-FLG = ZERO
         PERFORM  MOVE-DNGROPF
         REWRITE  DNG-REC
     ELSE
         DISPLAY
         NC"未登録です　送受信グループ_＝" DSP-SGRPNO UPON CONS
     END-IF.

 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*    送受信グループマスタ更新　処理区分＝３（削除）2.4.3     *
****************************************************************
 FILE-DLT-SEC           SECTION.
     MOVE  "FILE-DLT-SEC"        TO  S-NAME.
*キー項目転送
*送受信データ種別
     MOVE  DSP-SGRPNO            TO  DNG-F01.
*送受信グループマスタＲＥＡＤ（存在すればレコード削除）
     PERFORM  DNGROPF-READ.
     IF  READ-FLG = ZERO
         DELETE  DNGROPF
     ELSE
         DISPLAY
         NC"未登録です　送受信グループ_＝" DSP-SGRPNO UPON CONS
     END-IF.
*
 FILE-DLT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.

     PERFORM  SDATE-GET-SEC.
*システム日付転送
     MOVE  GAMEN-HIDUKE           TO  DSP-SYSDT.
*システム時間転送
     MOVE  GAMEN-TIME             TO  DSP-SYSTIM.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
*処理区分
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-PFMSG
*キー項目
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-PFMSG
*明細項目
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFMSG
*確認
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-PFMSG
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-PFMSG
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FVM00201"          TO   DSP-FMT.
     WRITE    DSP-FVM00201.
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
*    MOVE    "SCREEN"             TO   DSP-GRP.
     EVALUATE   PSW
*処理区分
         WHEN   "1"
                MOVE    "GRP001"  TO   DSP-GRP
*キー項目
         WHEN   "2"
                MOVE    "GRP002"  TO   DSP-GRP
*明細項目
         WHEN   "3"
                MOVE    "GRP003"  TO   DSP-GRP
*確認
         WHEN   "4"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FVM00201"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             項目転送  （ＥＤＩメッセージマスタ → 画面）
****************************************************************
 MOVE-DSP-SEC           SECTION.
     MOVE  "MOVE-DSP-SEC"   TO  S-NAME.

     MOVE  DNG-F01          TO  DSP-SGRPNO.
     MOVE  DNG-F02          TO  DSP-GRPNAM.
     MOVE  DNG-F03          TO  DSP-DSYU1.
     IF    DNG-F03   NOT = SPACE
           MOVE   DNG-F03     TO  WRK-DSYU
           PERFORM  DNDATSF-READ
           IF  READ-FLG  =  0
               MOVE  DND-F02  TO  DSP-DNAM1
           END-IF
     END-IF.
     MOVE  DNG-F04          TO  DSP-DSYU2.
     IF    DNG-F04   NOT = SPACE
           MOVE   DNG-F04     TO  WRK-DSYU
           PERFORM  DNDATSF-READ
           IF  READ-FLG  =  0
               MOVE  DND-F02  TO  DSP-DNAM2
           END-IF
     END-IF.
     MOVE  DNG-F05          TO  DSP-DSYU3.
     IF    DNG-F05   NOT = SPACE
           MOVE   DNG-F05     TO  WRK-DSYU
           PERFORM  DNDATSF-READ
           IF  READ-FLG  =  0
               MOVE  DND-F02  TO  DSP-DNAM3
           END-IF
     END-IF.
     MOVE  DNG-F06          TO  DSP-DSYU4.
     IF    DNG-F06   NOT = SPACE
           MOVE   DNG-F06     TO  WRK-DSYU
           PERFORM  DNDATSF-READ
           IF  READ-FLG  =  0
               MOVE  DND-F02  TO  DSP-DNAM4
           END-IF
     END-IF.
     MOVE  DNG-F07          TO  DSP-DSYU5.
     IF    DNG-F07   NOT = SPACE
           MOVE   DNG-F07     TO  WRK-DSYU
           PERFORM  DNDATSF-READ
           IF  READ-FLG  =  0
               MOVE  DND-F02  TO  DSP-DNAM5
           END-IF
     END-IF.
*
 MOVE-DSP-EXIT.
     EXIT.
****************************************************************
*             項目転送  （画面 → ＥＤＩメッセージマスタ）
****************************************************************
 MOVE-DNGROPF        SECTION.
     MOVE  "MOVE-DNGROPF-SEC"    TO  S-NAME.

     PERFORM  SDATE-GET-SEC.

     MOVE  DSP-SGRPNO       TO  DNG-F01.
     MOVE  DSP-GRPNAM       TO  DNG-F02.
     MOVE  DSP-DSYU1        TO  DNG-F03.
     MOVE  DSP-DSYU2        TO  DNG-F04.
     MOVE  DSP-DSYU3        TO  DNG-F05.
     MOVE  DSP-DSYU4        TO  DNG-F06.
     MOVE  DSP-DSYU5        TO  DNG-F07.

     PERFORM  SDATE-GET-SEC.
     IF  DSP-SYORI = 1 *> 1:登録処理
*        スケジュール作成日，時間
         MOVE  PAR-TANCD         TO  DNG-F94
         MOVE  SYS-DATE          TO  DNG-F95
         MOVE  WK-TIME(1:6)      TO  DNG-F96
     ELSE              *> 2:更新処理
*        スケジュール更新日，時間
         MOVE  PAR-TANCD         TO  DNG-F97
         MOVE  SYS-DATE          TO  DNG-F98
         MOVE  WK-TIME(1:6)      TO  DNG-F99
     END-IF.
*
 MOVE-DNDATSF-EXIT.
     EXIT.
****************************************************************
*             送受信データ種別マスタ　ＲＥＡＤ                 *
****************************************************************
 DNDATSF-READ             SECTION.
     MOVE     "DNDATSF-READ"      TO   S-NAME.
*
     MOVE    ZERO                 TO   READ-FLG.
     MOVE    ZERO                 TO   SAKKBN-FLG.
*送受信データ種別マスタＲＥＡＤ（該当データ無時，READ-FLG=1）
*    MOVE    DSP-DSYU1            TO   DND-F01.
     MOVE    WRK-DSYU             TO   DND-F01.
     READ    DNDATSF
         INVALID
             MOVE    1            TO   READ-FLG
     END-READ.
*
 DNDATSF-READ-EXIT.
     EXIT.
****************************************************************
*             送受信グループマスタ　ＲＥＡＤ                   *
****************************************************************
 DNGROPF-READ             SECTION.
     MOVE     "DNGROPF-READ"      TO   S-NAME.
*
     MOVE    ZERO                 TO   READ-FLG.
     MOVE    ZERO                 TO   SAKKBN-FLG.
*送受信グループマスタＲＥＡＤ（該当データ無時，READ-FLG=1）
     MOVE    DSP-SGRPNO           TO   DNG-F01.
     READ    DNGROPF
         INVALID
             MOVE    1            TO   READ-FLG
     END-READ.
*
 DNGROPF-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"        TO  S-NAME.
*画面の初期化
     MOVE  SPACE                 TO  DSP-FVM00201.
*日付／時刻取得＝＞編集
     PERFORM  SDATE-GET-SEC.
*システム日付転送
     MOVE  GAMEN-HIDUKE          TO  DSP-SYSDT.
*システム時間転送
     MOVE  GAMEN-TIME            TO  DSP-SYSTIM.
*リバース，カーソルパーク解除
***  送受信グループ_
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SGRPNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SGRPNO.
***  送受信グループ名
     MOVE  SPACE            TO                 DSP-GRPNAM.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-GRPNAM.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GRPNAM.
***  データ種別１
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DSYU1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DSYU1.
***  データ種別２
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DSYU2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DSYU2.
***  データ種別３
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DSYU3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DSYU3.
***  データ種別４
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DSYU4.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DSYU4.
***  データ種別５
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DSYU5.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DSYU5.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DNGROPF  DNDATSF  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  NVM0020I   END PROGRAM  >>******************
