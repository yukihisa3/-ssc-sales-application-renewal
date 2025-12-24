# SSY3870I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3870I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　ＴＲＡＮＴＲＡＮ連携              *
*    モジュール名　　　　：　連携データ作成指示                *
*    作成日／更新日　　　：　2011/12/02                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　管理番号を指定して、連携データを　*
*                        ：　抽出する。　　　　　　　　　　    *
*    変更日　　　　　　　：　2015/04/13                        *
*    変更者　　　　　　　：　NAV INOUE                         *
*    変更概要　　　　　　：　指示画面に作場を追加　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3870I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         11/12/02.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*箱数ファイル
     SELECT  NFHAKOF   ASSIGN    TO        NFHAKOL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       HK1-F01
                                           HK1-F05
                                           HK1-F06
                                           HK1-F07
                                           HK1-F08
                       FILE      STATUS    HK1-ST.
*数量訂正ファイル
     SELECT  NFSUTEF   ASSIGN    TO        NFSUTEL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       ST1-F01
                                           ST1-F05
                                           ST1-F06
                                           ST1-F07
                                           ST1-F08
                                           ST1-F09
                       FILE      STATUS    ST1-ST.
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
*    FILE = 箱数ファイル                                       *
****************************************************************
 FD  NFHAKOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFHAKOF   OF   XFDLIB
                       JOINING   HK1       AS   PREFIX.
****************************************************************
*    FILE = 数量訂正ファイル                                   *
****************************************************************
 FD  NFSUTEF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFSUTEF   OF   XFDLIB
                       JOINING   ST1       AS   PREFIX.
****************************************************************
*  FILE= 作場マスタ                                          *
****************************************************************
 FD  SAKUBAF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF    OF   XFDLIB
                       JOINING   SAK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY38701  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  HK1-ST                   PIC  X(02).
     03  ST1-ST                   PIC  X(02).
     03  SAK-ST                   PIC  X(02).
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
             VALUE NC"この管理番号は抽出済です。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"抽出対象データが存在しません。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"作場マスタに登録されていません".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"作場コードを指定して下さい".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"この管理番号は発注確定されていません。".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  9   PIC   N(20).
*
 01  FILE-ERR.
     03  HK1-ERR           PIC N(20) VALUE
                     NC"箱数確定ファイルエラー".
     03  ST1-ERR           PIC N(20) VALUE
                     NC"数量確定ファイルエラー".
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
 01  LINK-KANRINO            PIC  9(08).
 01  LINK-SAKUBA             PIC  X(02).
 01  LINK-KBN                PIC  9(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING  LINK-KANRINO
                                         LINK-SAKUBA
                                         LINK-KBN.
**************************************************************
 DECLARATIVES.
 HK1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFHAKOF.
     DISPLAY     HK1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HK1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ST1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSUTEF.
     DISPLAY     ST1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ST1-ST    UPON      CONS.
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
     OPEN     INPUT     NFHAKOF  NFSUTEF  SAKUBAF.
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
                PERFORM   PARA-CHK-SEC
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
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK-SEC             SECTION.
     MOVE     "PARA-CHK-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*管理番号チェック
     IF       DSP-KANRNO NOT NUMERIC
         OR   DSP-KANRNO =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KANRNO
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KANRNO
              GO             TO   PARA-CHK-EXIT
     END-IF.
*
*作場コードチェック
***  未入力時，作場コード＝０
     IF   DSP-SKBCD = SPACE
          IF  DSP-KANRNO(1:1)    =    9
              MOVE     SPACE      TO   DSP-SKBCD
          ELSE
              MOVE   6     TO   ERR-FLG
              MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SKBCD
              MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SKBCD
              GO           TO   PARA-CHK-EXIT
          END-IF
     ELSE
         IF   DSP-SKBCD   =   "00"
           IF DSP-KANRNO(1:1)    =    9
              CONTINUE
           ELSE
              MOVE   6     TO   ERR-FLG
              MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SKBCD
              MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SKBCD
              GO           TO   PARA-CHK-EXIT
           END-IF
         ELSE
***  本発は作場入力しても無視
           IF DSP-KANRNO(1:1)    =    9
              MOVE      SPACE         TO   DSP-SKBCD
              MOVE      SPACE         TO   DSP-SKBMEI
           ELSE
***  作場マスタＲＥＡＤ
              MOVE      DSP-SKBCD     TO   SAK-F01
              READ      SAKUBAF
              INVALID
                     MOVE   5     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SKBCD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SKBCD
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  SAK-F02     TO               DSP-SKBMEI
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SKBCD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SKBCD
              END-READ
           END-IF
         END-IF
     END-IF.
*
*箱数確定ファイル存在／未抽出チェック
*スタート
     MOVE     SPACE          TO   HK1-REC.
     INITIALIZE                   HK1-REC.
     MOVE     DSP-KANRNO     TO   HK1-F01.
     MOVE     DSP-SKBCD      TO   HK1-F05.
     START    NFHAKOF   KEY  IS   >=   HK1-F01  HK1-F05  HK1-F06
                                       HK1-F07  HK1-F08
     INVALID
              MOVE      4    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-KANRNO
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-KANRNO
              IF  DSP-KANRNO(1:1) NOT = 9
                  MOVE "R"   TO   EDIT-OPTION OF DSP-SKBCD
              END-IF
              GO             TO   PARA-CHK-EXIT
     NOT INVALID
***           箱数確定ファイル読込
              READ     NFHAKOF
              END-READ
***           管理番号が画面と同一かチェック
              IF   DSP-KANRNO  =  HK1-F01
*04/13*************IF  HK1-F98 =  SPACE
                   IF  DSP-KANRNO(1:1) = 9
                       IF  HK1-F98 =  SPACE
                           MOVE SPACE TO EDIT-CURSOR OF DSP-KANRNO
                       ELSE
                           MOVE   2  TO ERR-FLG
                           MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
                           MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
                           GO        TO PARA-CHK-EXIT
                       END-IF
                       IF  HK1-F93 =  "1"
                           MOVE SPACE TO EDIT-CURSOR OF DSP-KANRNO
                       ELSE
                           MOVE   7  TO ERR-FLG
                           MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
                           MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
                           GO        TO PARA-CHK-EXIT
                       END-IF
                   ELSE
                       IF  HK1-F05 =  DSP-SKBCD
                           IF  HK1-F98 =  SPACE
                               MOVE SPACE TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                           ELSE
                               MOVE   2  TO ERR-FLG
                               MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-KANRNO
                               MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKBCD
                               MOVE  "C" TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                               GO        TO PARA-CHK-EXIT
                           END-IF
                           IF  HK1-F93 =  "1"
                               MOVE SPACE TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                           ELSE
                               MOVE   7  TO ERR-FLG
                               MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-KANRNO
                               MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKBCD
                               MOVE  "C" TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                               GO        TO PARA-CHK-EXIT
                           END-IF
                       ELSE
                           MOVE   4  TO ERR-FLG
                           MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
                           MOVE  "R" TO EDIT-OPTION OF DSP-SKBCD
                           MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
                           GO        TO PARA-CHK-EXIT
                       END-IF
                   END-IF
*04/13*************ELSE
*                      MOVE   2   TO   ERR-FLG
*                      MOVE  "R"  TO   EDIT-OPTION OF DSP-KANRNO
*                      MOVE  "C"  TO   EDIT-CURSOR OF DSP-KANRNO
*                      GO         TO   PARA-CHK-EXIT
*04/13*************END-IF
              ELSE
                   MOVE   4       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION OF DSP-KANRNO
                   MOVE  "C"      TO   EDIT-CURSOR OF DSP-KANRNO
                   IF  DSP-KANRNO(1:1) NOT = 9
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKBCD
                   END-IF
                   GO             TO   PARA-CHK-EXIT
              END-IF
     END-START.
*数量確定ファイル存在／未抽出チェック
*スタート
     MOVE     SPACE          TO   ST1-REC.
     INITIALIZE                   ST1-REC.
     MOVE     DSP-KANRNO     TO   ST1-F01.
     MOVE     DSP-SKBCD      TO   ST1-F05.
     START    NFSUTEF   KEY  IS   >=   ST1-F01  ST1-F05  ST1-F06
                                       ST1-F07  ST1-F08  ST1-F09
     INVALID
              MOVE      4    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-KANRNO
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-KANRNO
              IF  DSP-KANRNO(1:1) NOT = 9
                  MOVE "R"   TO   EDIT-OPTION OF DSP-SKBCD
              END-IF
              GO             TO   PARA-CHK-EXIT
     NOT INVALID
***           数量確定ファイル読込
              READ     NFSUTEF
              END-READ
***           管理番号が画面と同一かチェック
              IF   DSP-KANRNO  =  ST1-F01
*04/13*************IF  ST1-F98 =  SPACE
                   IF  DSP-KANRNO(1:1) = 9
                       IF  ST1-F98 =  SPACE
                           MOVE SPACE TO EDIT-CURSOR OF DSP-KANRNO
                       ELSE
                           MOVE   2  TO ERR-FLG
                           MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
                           MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
                           GO        TO PARA-CHK-EXIT
                       END-IF
                       IF  ST1-F91 =  "1"
                           MOVE SPACE TO EDIT-CURSOR OF DSP-KANRNO
                       ELSE
                           MOVE   7  TO ERR-FLG
                           MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
                           MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
                           GO        TO PARA-CHK-EXIT
                       END-IF
                   ELSE
                       IF  ST1-F05 =  DSP-SKBCD
                           IF  ST1-F98 =  SPACE
                               MOVE SPACE TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                           ELSE
                               MOVE   2  TO ERR-FLG
                               MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-KANRNO
                               MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKBCD
                               MOVE  "C" TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                               GO        TO PARA-CHK-EXIT
                           END-IF
                           IF  ST1-F91 =  "1"
                               MOVE SPACE TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                           ELSE
                               MOVE   7  TO ERR-FLG
                               MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-KANRNO
                               MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKBCD
                               MOVE  "C" TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                               GO        TO PARA-CHK-EXIT
                           END-IF
                       ELSE
                           MOVE   4  TO ERR-FLG
                           MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
                           MOVE  "R" TO EDIT-OPTION OF DSP-SKBCD
                           MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
                           GO        TO PARA-CHK-EXIT
                       END-IF
                   END-IF
*04/13*************ELSE
*                      MOVE   2   TO   ERR-FLG
*                      MOVE  "R"  TO   EDIT-OPTION OF DSP-KANRNO
*                      MOVE  "C"  TO   EDIT-CURSOR OF DSP-KANRNO
*                      GO         TO   PARA-CHK-EXIT
*04/13*************END-IF
              ELSE
*04/13*違うはず*→*MOVE   2       TO   ERR-FLG
                   MOVE   4       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION OF DSP-KANRNO
                   MOVE  "C"      TO   EDIT-CURSOR OF DSP-KANRNO
                   IF  DSP-KANRNO(1:1) NOT = 9
                       MOVE  "R" TO   EDIT-OPTION OF DSP-SKBCD
                   END-IF
                   GO             TO   PARA-CHK-EXIT
              END-IF
     END-START.
*
     MOVE    "2"       TO    PSW.
*
 PARA-CHK-EXIT.
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
                MOVE    DSP-KANRNO    TO   LINK-KANRINO
                MOVE    DSP-SKBCD     TO   LINK-SAKUBA
*04/13**********ＯＮＬ／手書判断
*               IF  HK1-F02 = 99999999
                IF  DSP-KANRNO(1:1) = 9
                    MOVE  2           TO   LINK-KBN
                ELSE
                    MOVE  1           TO   LINK-KBN
                END-IF
****************
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
     MOVE    "FSY38701"          TO   DSP-FMT.
     WRITE    DSP-FSY38701.
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
                MOVE    "PARGRP"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "CHKGRP"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY38701"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY38701.
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
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KANRNO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KANRNO.
***  作場コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBCD.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             NFHAKOF  NFSUTEF  SAKUBAF  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3870I   END PROGRAM  >>******************

```
