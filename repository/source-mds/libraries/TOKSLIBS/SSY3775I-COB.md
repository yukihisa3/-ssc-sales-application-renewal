# SSY3775I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3775I.COB`

## ソースコード

```cobol
***********************************************************
*                                                         *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　 *
*    サブシステム　　　　：　ナフコＥＤＩ受信システム　   *
*    業務名　　　　　　　：　ナフコＥＤＩ受信             *
*    モジュール名　　　　：　作場マスタメンテナンス       *
*    作成日／更新日　　　：　2010/10/04                   *
*    作成者／更新者　　　：　ＮＡＶ飯田                   *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　 *
*                                                         *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3775I.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2010/10/04.
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
*作場マスタ
     SELECT  SAKUBAF   ASSIGN    TO        SAKUBAL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SKB-F01
                       FILE      STATUS    SKB-ST.
*
*条件マスタ
     SELECT  HJYOKEN   ASSIGN    TO        JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JYK-F01
                                           JYK-F02
                       FILE      STATUS    JYK-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY37751  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
*    FILE = 作場マスタ                                         *
****************************************************************
 FD  SAKUBAF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SKB       AS   PREFIX.
****************************************************************
*    FILE = 条件マスタ                                         *
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYK       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  SKB-ST                   PIC  X(02).
     03  JYK-ST                   PIC  X(02).
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
     03  FG-HJYOKEN-INV           PIC  9(01)  VALUE  ZERO.
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
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
*受信日付チェック用
* 01  JUSIN-DATE.
*    03  JUSIN-DATEY              PIC  9(04)  VALUE  ZERO.
*    03  JUSIN-DATEM              PIC  9(02)  VALUE  ZERO.
*    03  JUSIN-DATED              PIC  9(02)  VALUE  ZERO.
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
         05  FILLER              PIC   N(25)
             VALUE NC"正しい値を入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(25)
             VALUE NC"作場名称を入力して下さい。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(25)
             VALUE
             NC"作場マスタに登録済です。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(25)
             VALUE
             NC"作場マスタに未登録です。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(25)
     VALUE NC"直送ＣＤを入力！！（空白：なし、１：直送有）".
     03  ERR-MSG6.
         05  FILLER              PIC   N(25)
             VALUE NC"無効キーです。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(25)
             VALUE NC"条件マスタに未登録です。".
     03  ERR-MSG8.
         05  FILLER              PIC   N(25)
             VALUE NC"作場ＣＤを入力して下さい。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(25)
         VALUE NC"直送先ＣＤを入力！！（連携用）".
     03  ERR-MSG10.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG11.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG12.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG13.
         05  FILLER              PIC   N(25)
             VALUE NC"処理区分を入力してください。".
     03  ERR-MSG14.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  14  PIC   N(25).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  SKB-ERR           PIC N(20) VALUE
                        NC"作場マスタエラー".
     03  JYK-ERR           PIC N(20) VALUE
                        NC"条件マスタエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 LINKAGE               SECTION.
****************************************************************
 01  PAR-BMNCD             PIC  X(04).
 01  PAR-TANCD             PIC  X(02).
*
**************************************************************
 PROCEDURE             DIVISION   USING  PAR-BMNCD  PAR-TANCD.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SKB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SKB-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SKB-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JYK-ST    UPON      CONS.
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
     OPEN  I-O   DSPFILE.
     OPEN  I-O   SAKUBAF.
     OPEN  INPUT HJYOKEN.
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
*キー項目入力（修正/削除）
         WHEN      "2"  PERFORM   DSP-HEAD2-SEC
*明細項目入力（修正/削除）
         WHEN      "3"  PERFORM   DSP-BODY-SEC
*明細項目入力（登録）
         WHEN      "5"  PERFORM   DSP-BODY2-SEC
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
     IF     DSP-KBN NOT NUMERIC
         OR DSP-KBN = ZERO
         MOVE  13                TO  ERR-FLG
         MOVE  SPACE             TO  DSP-KBN (1:1)
         GO TO  HEAD1-CHK-EXIT
     END-IF.
*処理区分＝１，２，３以外はエラー
     IF  DSP-KBN = 1 OR  2  OR  3
         IF  DSP-KBN = 1   *> 登録
             MOVE  "5"           TO  PSW
         ELSE              *> 修正/削除
             MOVE  "2"           TO  PSW
         END-IF

*        同一モードでループさせるため
         MOVE  DSP-KBN           TO  SAV-SHORI
     ELSE
         MOVE  1                 TO  ERR-FLG
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
***  メッセージＮＯ
     IF  DSP-SKBACD = SPACE
         MOVE   8           TO  ERR-FLG
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SKBACD
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SKBACD
         GO TO  HEAD2-CHK-EXIT
     ELSE
         MOVE  "M"          TO  EDIT-OPTION OF DSP-SKBACD
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-SKBACD
     END-IF.

*作場マスタ ＲＥＡＤ
     PERFORM  SAKUBAF-READ.
***  処理区分＝１（登録）
     IF       DSP-KBN   =   1
***           データが存在すればエラー
              IF    READ-FLG   =   ZERO
                    MOVE     3            TO   ERR-FLG
***           データが存在しなければＯＫ（ボディ入力へ）
*****         ELSE
*****               MOVE    "3"           TO   PSW
              END-IF
     ELSE
***  処理区分＝２（修正），３（削除）
*           IF    SAKKBN-FLG  = 1
***           データが存在すればＯＫ
              IF  READ-FLG   =   ZERO
                  PERFORM   MOVE-DSP-SEC
***               処理区分＝２（修正）は，ボディ入力へ
                  IF  DSP-KBN  =  2
                      MOVE    "3"          TO   PSW
                  ELSE
***                   処理区分＝３（削除）は，確認入力へ
                      MOVE    "4"          TO   PSW
                  END-IF
***         データが存在しなければエラー
            ELSE
                  MOVE     4            TO   ERR-FLG
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
*作場名称
     EVALUATE  TRUE
**     WHEN  EDIT-STATUS OF DSP-SAKBNM NOT = SPACE
**       MOVE   1           TO  ERR-FLG
**       MOVE  "R"          TO  EDIT-OPTION OF DSP-SAKBNM
**       MOVE  "C"          TO  EDIT-CURSOR OF DSP-SAKBNM
**       GO TO  BODY-CHK-EXIT
**
       WHEN  DSP-SAKBNM = SPACE
         MOVE   2           TO  ERR-FLG
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SAKBNM
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SAKBNM
         GO TO  BODY-CHK-EXIT

       WHEN  OTHER
         MOVE  "M"          TO  EDIT-OPTION OF DSP-SAKBNM
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-SAKBNM
     END-EVALUATE.

*直送ＣＤ
*****MOVE  SPACE            TO  DSP-ROOTNM.
     IF  DSP-ROOTCD  =  SPACE  OR  "1"
         MOVE  "M"          TO  EDIT-OPTION OF DSP-ROOTCD
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-ROOTCD
     ELSE
         MOVE   5           TO  ERR-FLG
         MOVE  "R"          TO  EDIT-OPTION OF DSP-ROOTCD
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-ROOTCD
         GO TO  BODY-CHK-EXIT
     END-IF.
*****IF  DSP-ROOTCD  = SPACE
*        MOVE   5           TO  ERR-FLG
*        MOVE  "R"          TO  EDIT-OPTION OF DSP-ROOTCD
*        MOVE  "C"          TO  EDIT-CURSOR OF DSP-ROOTCD
*        GO TO  BODY-CHK-EXIT
*    ELSE
*        MOVE  89           TO  JYK-F01
*        MOVE  DSP-ROOTCD   TO  JYK-F02
*        PERFORM  RD-JYOKEN-SEC
*        IF  FG-HJYOKEN-INV = ZERO
*            MOVE  JYK-F03  TO  DSP-ROOTNM
*        ELSE
*            MOVE  SPACE    TO  DSP-ROOTNM
*            MOVE   7       TO  ERR-FLG
*            MOVE  "R"      TO  EDIT-OPTION OF DSP-ROOTCD
*            MOVE  "C"      TO  EDIT-CURSOR OF DSP-ROOTCD
*            GO TO  BODY-CHK-EXIT
*        END-IF
*
*        MOVE  "M"          TO  EDIT-OPTION OF DSP-ROOTCD
*        MOVE  SPACE        TO  EDIT-CURSOR OF DSP-ROOTCD
*****END-IF.
*直送ＣＤ
     IF  DSP-TYOKUS  NOT  NUMERIC
         MOVE   6           TO  ERR-FLG
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TYOKUS
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TYOKUS
         GO TO  BODY-CHK-EXIT
     ELSE
         MOVE  "M"          TO  EDIT-OPTION OF DSP-TYOKUS
         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-TYOKUS
     END-IF.

     MOVE  "4"                   TO  PSW.

 BODY-CHK-EXIT.
     EXIT.

****************************************************************
*    条件マスタ検索                                            *
****************************************************************
 RD-JYOKEN-SEC          SECTION.
     READ  HJYOKEN
       INVALID
         MOVE  1                 TO  FG-HJYOKEN-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HJYOKEN-INV
     END-READ.

 RD-JYOKEN-EXIT.
     EXIT.

****************************************************************
*             明細項目・登録　入力( PSW = 5 )                  *
****************************************************************
 DSP-BODY2-SEC          SECTION.
     MOVE     "DSP-BODY2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD2-CHK-SEC
                IF  ERR-FLG = ZERO
                    PERFORM   BODY-CHK-SEC
                END-IF
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
                GO       TO      DSP-BODY-SEC
     END-EVALUATE.
*
 DSP-BODY2-EXIT.
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
                EVALUATE  DSP-KBN
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
                MOVE   SAV-SHORI TO   DSP-KBN
                IF DSP-KBN  = 1  *> 登録
                   MOVE  "5"     TO  PSW
                ELSE               *> 修正/削除
                   MOVE  "2"     TO  PSW
                END-IF
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                EVALUATE  DSP-KBN
                  WHEN  1      *> 登録
                    MOVE    "5"       TO   PSW
                  WHEN  2      *> 修正
                    MOVE    "3"       TO   PSW
                  WHEN  OTHER  *> 削除
                    MOVE    "1"       TO   PSW
                END-EVALUATE
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
*    作場マスタ更新　処理区分＝１（登録）2.4.1     *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE  "FILE-WRT-SEC"         TO  S-NAME.
*レコード初期クリア
     MOVE  SPACE                  TO  SKB-REC.
     INITIALIZE  SKB-REC.
*キー項目転送
     MOVE  DSP-SKBACD             TO  SKB-F01.
*明細項目
     PERFORM  MOVE-SAKUBAF.
*作場マスタ登録
     WRITE    SKB-REC.
*
 FILE-WRT-EXIT.
     EXIT.
****************************************************************
*    作場マスタ更新　処理区分＝２（修正）2.4.2     *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE  "FILE-UPD-SEC"        TO  S-NAME.
*キー項目転送
     MOVE  DSP-SKBACD            TO  SKB-F01.
*作場マスタ ＲＥＡＤ（存在すればレコード更新）
     PERFORM  SAKUBAF-READ.
     IF  READ-FLG = ZERO
         PERFORM  MOVE-SAKUBAF
         REWRITE  SKB-REC
     ELSE
         DISPLAY
           NC"未登録です　ＭＳＧＮＯ＝"  DSP-SKBACD  UPON CONS
     END-IF.

 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*    作場マスタ更新　処理区分＝３（削除）2.4.3     *
****************************************************************
 FILE-DLT-SEC           SECTION.
     MOVE  "FILE-DLT-SEC"        TO  S-NAME.
*キー項目転送
***  メッセージＮＯ
     MOVE  DSP-SKBACD            TO  SKB-F01.
*作場マスタＲＥＡＤ（存在すればレコード削除）
     PERFORM  SAKUBAF-READ.
     IF  READ-FLG = ZERO
         DELETE  SAKUBAF
     ELSE
         DISPLAY
           NC"未登録です　作場ＣＤ＝"  DSP-SKBACD  UPON CONS
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
     MOVE  SYS-DATE              TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.
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
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
*キー項目
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
*明細項目
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
*明細項目（登録）
         WHEN   "5"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
*確認
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY37751"          TO   DSP-FMT.
     WRITE    DSP-FSY37751.
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
                MOVE    "SMODE"   TO   DSP-GRP
*キー項目
         WHEN   "2"
                MOVE    "KEYGRP"  TO   DSP-GRP
*明細項目
         WHEN   "3"
                MOVE    "MAIN"    TO   DSP-GRP
*明細項目（登録）
         WHEN   "5"
                MOVE    "MAIN2"   TO   DSP-GRP
*確認
         WHEN   "4"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FSY37751"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             項目転送  （作場マスタ → 画面）
****************************************************************
 MOVE-DSP-SEC           SECTION.
     MOVE  "MOVE-DSP-SEC"   TO  S-NAME.

     MOVE  SKB-F01          TO  DSP-SKBACD.
     MOVE  SKB-F02          TO  DSP-SAKBNM.
     MOVE  SKB-F03          TO  DSP-ROOTCD.
     MOVE  SKB-F04          TO  DSP-TYOKUS.
     MOVE  89               TO  JYK-F01.
     MOVE  DSP-ROOTCD       TO  JYK-F02.
     PERFORM  RD-JYOKEN-SEC.
     IF  FG-HJYOKEN-INV = ZERO
         MOVE  JYK-F03      TO  DSP-ROOTNM
     ELSE
         MOVE  SPACE        TO  DSP-ROOTNM
     END-IF.
*
 MOVE-DSP-EXIT.
     EXIT.
****************************************************************
*             項目転送  （画面 → 作場マスタ）
****************************************************************
 MOVE-SAKUBAF        SECTION.
     MOVE  "MOVE-SAKUBAF-SEC"    TO  S-NAME.

     PERFORM  SDATE-GET-SEC.

     MOVE  DSP-SKBACD       TO  SKB-F01.
     MOVE  DSP-SAKBNM       TO  SKB-F02.
     MOVE  DSP-ROOTCD       TO  SKB-F03.
     MOVE  DSP-TYOKUS       TO  SKB-F04.

     PERFORM  SDATE-GET-SEC.
     IF  DSP-KBN = 1 *> 1:登録処理
*        スケジュール作成日，時間
         MOVE  PAR-BMNCD         TO  SKB-F92
         MOVE  PAR-TANCD         TO  SKB-F93
         MOVE  SYS-DATE          TO  SKB-F94
         MOVE  WK-TIME(1:4)      TO  SKB-F95
         MOVE  SPACE             TO  SKB-F96
         MOVE  SPACE             TO  SKB-F97
         MOVE  ZERO              TO  SKB-F98
         MOVE  ZERO              TO  SKB-F99
     ELSE              *> 2:更新処理
*        スケジュール更新日，時間
         MOVE  PAR-BMNCD         TO  SKB-F96
         MOVE  PAR-TANCD         TO  SKB-F97
         MOVE  SYS-DATE          TO  SKB-F98
         MOVE  WK-TIME(1:4)      TO  SKB-F99
     END-IF.
*
 MOVE-SAKUBAF-EXIT.
     EXIT.
****************************************************************
*             作場マスタ　ＲＥＡＤ                             *
****************************************************************
 SAKUBAF-READ             SECTION.
     MOVE     "SAKUBAF-READ"      TO   S-NAME.
*
     MOVE    ZERO                 TO   READ-FLG.
     MOVE    ZERO                 TO   SAKKBN-FLG.
*作場マスタＲＥＡＤ（該当データ無時，READ-FLG=1）
     MOVE    DSP-SKBACD           TO   SKB-F01.
     READ    SAKUBAF
         INVALID
             MOVE    1            TO   READ-FLG
*        NOT INVALID
*            作成区分＝1（未作成）
*            IF    SKB-F01   =    "1"
*                  MOVE      1    TO   SAKKBN-FLG
*            END-IF
     END-READ.
*
 SAKUBAF-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"        TO  S-NAME.
*画面の初期化
     MOVE  SPACE                 TO  DSP-FSY37751.
*ＰＧＩＤ
     MOVE  "SSY3775I"           TO   DSP-PGID.
*システム日付転送
     MOVE  SYS-DATE              TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.
*リバース，カーソルパーク解除
***  メッセージＮＯ
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SKBACD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SKBACD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SAKBNM.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKBNM.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-ROOTCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-ROOTCD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TYOKUS.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TYOKUS.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  SAKUBAF.
     CLOSE  HJYOKEN.
     CLOSE  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3775I   END PROGRAM  >>******************

```
