# SSY3816I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3816I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　オンライン業務／本発業務　        *
*    モジュール名　　　　：　発注関係資料　　　　　　　　　　　*
*    　　　　　　　　　　：　（出力指示）　　　　　　　　　　　*
*    作成日／更新日　　　：　2015/03/31                        *
*    作成者／更新者　　　：　INOUE                             *
*    処理概要　　　　　　：　発注関係資料の出力指示　　　　　　*
*    更新日／更新者　　　：　2015/04/24 井上
*    修正概要　　　　　　：　_出力パターン追加
*                        ：　    3:発注CSV出力→3:確定前
*                        ：　                 4:確定後
*            　　　　　　：　_パラメタＯＵＴ追加
*                        ：　    オンライン／手書区分
*                          　                                  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3816I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2015/03/31.
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
*基本情報ファイル
     SELECT  NFJOHOF   ASSIGN    TO        NFJOHOL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       JOH-F01
                                           JOH-F05
                                           JOH-F06
                                           JOH-F07
                                           JOH-F08
                                           JOH-F09
                       FILE      STATUS    JOH-ST.
*本発基本情報ファイル
     SELECT  NHJOHOF   ASSIGN    TO        NHJOHOL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       NHJ-F01
                                           NHJ-F05
                                           NHJ-F06
                                           NHJ-F07
                                           NHJ-F08
                                           NHJ-F09
                       FILE      STATUS    NHJ-ST.
*作場マスタ
     SELECT  SAKUBAF   ASSIGN    TO        SAKUBAL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SAK-F01
                       FILE      STATUS    SAK-ST.
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
*    FILE = 基本情報ファイル                     *
****************************************************************
 FD  NFJOHOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFJOHOF   OF   XFDLIB
                       JOINING   JOH       AS   PREFIX.
****************************************************************
*    FILE = 本発基本情報ファイル                     *
****************************************************************
 FD  NHJOHOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NHJOHOF   OF   XFDLIB
                       JOINING   NHJ       AS   PREFIX.
****************************************************************
*  FILE= 作場マスタ                                          *
****************************************************************
 FD  SAKUBAF
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF    OF   XFDLIB
                       JOINING   SAK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY38161  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JOH-ST                   PIC  X(02).
     03  NHJ-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  SAK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SSY3816I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FSY38161".
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
             VALUE NC"管理番号を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"発注確定処理済み管理番号ではありません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"発注確定処理済み管理番号です".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"作場コードを入力してください".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"作場マスタに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"本発基本情報ファイルに登録されていません".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"基本情報ファイルに登録されていません".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい出力パターンを指定してください".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  10  PIC   N(20).
*
 01  FILE-ERR.
     03  JOH-ERR           PIC N(20) VALUE
                        NC"基本情報ファイルエラー".
     03  NHJ-ERR           PIC N(20) VALUE
                        NC"本発基本情報ファイルエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  SAK-ERR           PIC N(20) VALUE
                        NC"作場マスタエラー".
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
 01  LINK-KANRINO        PIC  9(08).
 01  LINK-SAKUBA-CD      PIC  X(02).
 01  LINK-SYUPTN         PIC  X(01).
*↓2015/04/24
 01  LINK-SYURUI         PIC  X(01).
*↑2015/04/24
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-KANRINO
                                     LINK-SAKUBA-CD
                                     LINK-SYUPTN
*↓2015/04/24
                                     LINK-SYURUI.
*↑2015/04/24
**************************************************************
*
 DECLARATIVES.
 JOH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF.
     DISPLAY     JOH-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JOH-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 NHJ-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NHJOHOF.
     DISPLAY     NHJ-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     NHJ-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SAK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SAK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SAK-ST    UPON      CONS.
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
     OPEN     INPUT     NFJOHOF   NHJOHOF   SAKUBAF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
     INITIALIZE   LINK-KANRINO
                  LINK-SAKUBA-CD
                  LINK-SYUPTN.
*↓2015/04/24
     INITIALIZE   LINK-SYURUI.
*↑2015/04/24
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
*パラメタ入力
         WHEN      "1"  PERFORM   DSP-PARA-SEC
*確認入力
         WHEN      "2"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-PARA-SEC         SECTION.
     MOVE     "DSP-PARA-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                IF   DSP-KANRI(1:1) NOT = 9
                     PERFORM   PARA-CHK1-SEC
                ELSE
                     PERFORM   PARA-CHK2-SEC
                END-IF
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
                GO       TO      DSP-PARA-SEC
     END-EVALUATE.
*
 DSP-PARA-EXIT.
     EXIT.
****************************************************************
*    パラメタチェック１（オンライン分）              2.1.1     *
****************************************************************
 PARA-CHK1-SEC             SECTION.
     MOVE     "PARA-CHK1-SEC"     TO   S-NAME.
*
*管理番号チェック
***  未入力チェック
     IF       DSP-KANRI  NOT NUMERIC
         OR   DSP-KANRI  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KANRI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KANRI
              GO             TO   PARA-CHK1-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-KANRI
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-KANRI
     END-IF.
*
*作場コードチェック
     IF       DSP-SKBCD    =   SPACE
              MOVE     SPACE      TO   DSP-SKBCD
     ELSE
         IF   DSP-SKBCD    =   "00"
              CONTINUE
         ELSE
***  本発は作場無視
           IF   DSP-KANRI(1:1)    =    9
                MOVE    SPACE          TO   DSP-SKBCD
           ELSE
***  オンラインは作場マスタチェック
              MOVE      DSP-SKBCD      TO   SAK-F01
              READ      SAKUBAF
              INVALID
                     MOVE   7     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SKBCD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SKBCD
                     GO           TO   PARA-CHK1-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SKBCD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SKBCD
              END-READ
         END-IF
     END-IF.
*
*基本情報ファイル存在チェック
     MOVE     SPACE          TO   JOH-REC.
     INITIALIZE                   JOH-REC.
     MOVE     DSP-KANRI      TO   JOH-F01.
     MOVE     DSP-SKBCD      TO   JOH-F05.
     MOVE     ZERO           TO   JOH-F06 JOH-F07 JOH-F08 JOH-F09.
     START    NFJOHOF   KEY  IS   >=   JOH-F01  JOH-F05  JOH-F06
                                       JOH-F07  JOH-F08  JOH-F09
     INVALID
              MOVE      9    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-KANRI
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-KANRI
              GO             TO   PARA-CHK1-EXIT
     NOT INVALID
***           ファイル存在チェック
              READ     NFJOHOF
              END-READ
***           作場コードが未入力の時は管理番号のみチェック
              IF   DSP-SKBCD   =  SPACE
                   IF  DSP-KANRI  =    JOH-F01
                       MOVE  SPACE TO  EDIT-CURSOR OF DSP-KANRI
                   ELSE
                       MOVE   9   TO   ERR-FLG
                       MOVE  "R"  TO   EDIT-OPTION OF DSP-KANRI
                       MOVE  "C"  TO   EDIT-CURSOR OF DSP-KANRI
                       GO         TO   PARA-CHK1-EXIT
                   END-IF
***           未入力以外は作場コードもチェック
              ELSE
                   IF  DSP-KANRI  =    JOH-F01 AND
                       DSP-SKBCD  =    JOH-F05
                       MOVE  SPACE TO  EDIT-CURSOR OF DSP-KANRI
                   ELSE
                       MOVE   9   TO   ERR-FLG
                       MOVE  "R"  TO   EDIT-OPTION OF DSP-KANRI
                       MOVE  "C"  TO   EDIT-CURSOR OF DSP-KANRI
                       GO         TO   PARA-CHK1-EXIT
                   END-IF
              END-IF
     END-START.
*
*出力パターンチェック
     IF    (  DSP-SYUPTN NOT =    "1"  )  AND
           (  DSP-SYUPTN NOT =    "2"  )  AND
           (  DSP-SYUPTN NOT =    "3"  )  AND
*↓2015/04/24
           (  DSP-SYUPTN NOT =    "4"  )
*↑2015/04/24
              MOVE     10    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SYUPTN
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SYUPTN
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SYUPTN
              GO             TO   PARA-CHK1-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SYUPTN
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SYUPTN
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-SYUPTN
     END-IF.
*
*発注確定処理済みチェック（確定後選択時）
*↓2015/04/24
*    IF    DSP-SYUPTN     =    "2"  OR   "3"
     IF    DSP-SYUPTN     =    "2"  OR   "4"
*↑2015/04/24
        IF    JOH-F34     =    "1"
              MOVE  SPACE TO  EDIT-CURSOR OF DSP-KANRI
        ELSE
              MOVE   2   TO   ERR-FLG
              MOVE  "C"  TO   EDIT-CURSOR OF DSP-KANRI
              GO         TO   PARA-CHK1-EXIT
        END-IF
     END-IF.
*
*発注確定処理済みチェック（確定前選択時）
*↓2015/04/24
*    IF    DSP-SYUPTN     =    "1"
     IF    DSP-SYUPTN     =    "1"  OR   "3"
*↑2015/04/24
        IF    JOH-F34     =    " "
              MOVE  SPACE TO  EDIT-CURSOR OF DSP-KANRI
        ELSE
              MOVE   4   TO   ERR-FLG
              MOVE  "C"  TO   EDIT-CURSOR OF DSP-KANRI
              GO         TO   PARA-CHK1-EXIT
        END-IF
     END-IF.
*
     MOVE    "2"       TO    PSW.
*
 PARA-CHK1-EXIT.
     EXIT.
****************************************************************
*    パラメタチェック２（本発／手書分）              2.1.1     *
****************************************************************
 PARA-CHK2-SEC             SECTION.
     MOVE     "PARA-CHK2-SEC"     TO   S-NAME.
*
*管理番号チェック
***  未入力チェック
     IF       DSP-KANRI  NOT NUMERIC
         OR   DSP-KANRI  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KANRI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KANRI
              GO             TO   PARA-CHK2-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-KANRI
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-KANRI
     END-IF.
*
*作場コードチェック
     IF       DSP-SKBCD    =   SPACE
              MOVE     SPACE      TO   DSP-SKBCD
     ELSE
         IF   DSP-SKBCD    =   "00"
              CONTINUE
         ELSE
***  本発は作場無視
              MOVE     SPACE      TO   DSP-SKBCD
         END-IF
     END-IF.
*
*本発基本情報ファイル存在チェック
     MOVE     SPACE          TO   NHJ-REC.
     INITIALIZE                   NHJ-REC.
     MOVE     DSP-KANRI      TO   NHJ-F01.
     MOVE     DSP-SKBCD      TO   NHJ-F05.
     MOVE     ZERO           TO   NHJ-F06 NHJ-F07 NHJ-F08 NHJ-F09
     START    NHJOHOF   KEY  IS   >=   NHJ-F01  NHJ-F05  NHJ-F06
                                       NHJ-F07  NHJ-F08  NHJ-F09
     INVALID
              MOVE      8    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-KANRI
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-KANRI
              GO             TO   PARA-CHK2-EXIT
     NOT INVALID
***           ファイル存在チェック
              READ     NHJOHOF
              END-READ
***           作場コードが未入力の時は管理番号のみチェック
              IF   DSP-SKBCD   =  SPACE
                   IF  DSP-KANRI  =    NHJ-F01
                       MOVE  SPACE TO  EDIT-CURSOR OF DSP-KANRI
                   ELSE
                       MOVE   8   TO   ERR-FLG
                       MOVE  "R"  TO   EDIT-OPTION OF DSP-KANRI
                       MOVE  "C"  TO   EDIT-CURSOR OF DSP-KANRI
                       GO         TO   PARA-CHK2-EXIT
                   END-IF
              END-IF
     END-START.
*
*出力パターンチェック
     IF    (  DSP-SYUPTN NOT =    "1"  )  AND
           (  DSP-SYUPTN NOT =    "2"  )  AND
           (  DSP-SYUPTN NOT =    "3"  )  AND
*↓2015/04/24
           (  DSP-SYUPTN NOT =    "4"  )
              MOVE     10    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SYUPTN
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SYUPTN
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SYUPTN
              GO             TO   PARA-CHK2-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SYUPTN
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SYUPTN
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-SYUPTN
     END-IF.
*
*発注確定処理済みチェック（確定後選択時）
*↓2015/04/24
*    IF    DSP-SYUPTN     =    "2"  OR   "3"
     IF    DSP-SYUPTN     =    "2"  OR   "4"
*↑2015/04/24
        IF    NHJ-F34     =    "1"
              MOVE  SPACE TO  EDIT-CURSOR OF DSP-KANRI
        ELSE
              MOVE   2   TO   ERR-FLG
              MOVE  "C"  TO   EDIT-CURSOR OF DSP-KANRI
              GO         TO   PARA-CHK2-EXIT
        END-IF
     END-IF.
*
*発注確定処理済みチェック（確定前選択時）
*↓2015/04/24
*    IF    DSP-SYUPTN     =    "1"
     IF    DSP-SYUPTN     =    "1"  OR   "3"
*↑2015/04/24
        IF    NHJ-F34     =    " "
              MOVE  SPACE TO  EDIT-CURSOR OF DSP-KANRI
        ELSE
              MOVE   4   TO   ERR-FLG
              MOVE  "C"  TO   EDIT-CURSOR OF DSP-KANRI
              GO         TO   PARA-CHK2-EXIT
        END-IF
     END-IF.
*
     MOVE    "2"       TO    PSW.
*
 PARA-CHK2-EXIT.
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
                MOVE    DSP-KANRI     TO   LINK-KANRINO
                MOVE    DSP-SKBCD     TO   LINK-SAKUBA-CD
                MOVE    DSP-SYUPTN    TO   LINK-SYUPTN
*↓2015/04/24
                IF      DSP-KANRI(1:1)     =   9
                        MOVE    "2"   TO   LINK-SYURUI
                ELSE
                        MOVE    "1"   TO   LINK-SYURUI
                END-IF
*↑2015/04/24
*        DISPLAY "LINK-KANRINO    = " LINK-KANRINO   UPON CONS
*        DISPLAY "LINK-SAKUBA-CD  = " LINK-SAKUBA-CD UPON CONS
                MOVE    "END"         TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
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
***      確認
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY38161"          TO   DSP-FMT.
     WRITE    DSP-FSY38161.
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
                MOVE    "GRP001"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY38161"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY38161.
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
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  管理番号
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KANRI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KANRI.
***  作場コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBCD.
***  出力パターン
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYUPTN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYUPTN.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             NFJOHOF  NHJOHOF   SAKUBAF  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3816I   END PROGRAM  >>******************

```
