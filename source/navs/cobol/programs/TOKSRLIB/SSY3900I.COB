****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　ナフコ送り状／納品明細書データ    *
*    　　　　　　　　　　　　作成指示　　　　　　　　　　　　  *
*    作成日／更新日　　　：　2019/12/03                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ナフコへ納品する資料発行データ　　*
*                        ：　の条件指示　　　　　　　　　　    *
*    変更日　　　　　　　：　    /  /                          *
*    変更者　　　　　　　：　                                  *
*    変更概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3900I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2019/12/03.
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
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SAK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY39001  OF   XMDLIB
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
     03  SAKUBAF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
***  作場コードｘ20
 01  WK-SAKU.
     03  WK-SAKUCD                PIC  X(02)  OCCURS 20.
 01  WK-SKBCD                     PIC  X(02)  VALUE  SPACE.
 01  IX                           PIC  9(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SSY3900I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FSY39001".
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
 01  LINK-SAKUBA.
     03  LINK-SAKU-01        PIC  X(02).
     03  LINK-SAKU-02        PIC  X(02).
     03  LINK-SAKU-03        PIC  X(02).
     03  LINK-SAKU-04        PIC  X(02).
     03  LINK-SAKU-05        PIC  X(02).
     03  LINK-SAKU-06        PIC  X(02).
     03  LINK-SAKU-07        PIC  X(02).
     03  LINK-SAKU-08        PIC  X(02).
     03  LINK-SAKU-09        PIC  X(02).
     03  LINK-SAKU-10        PIC  X(02).
     03  LINK-SAKU-11        PIC  X(02).
     03  LINK-SAKU-12        PIC  X(02).
     03  LINK-SAKU-13        PIC  X(02).
     03  LINK-SAKU-14        PIC  X(02).
     03  LINK-SAKU-15        PIC  X(02).
     03  LINK-SAKU-16        PIC  X(02).
     03  LINK-SAKU-17        PIC  X(02).
     03  LINK-SAKU-18        PIC  X(02).
     03  LINK-SAKU-19        PIC  X(02).
     03  LINK-SAKU-20        PIC  X(02).
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
     INITIALIZE         FLG-AREA
                        LINK-SAKU-01
                        LINK-SAKU-02
                        LINK-SAKU-03
                        LINK-SAKU-04
                        LINK-SAKU-05
                        LINK-SAKU-06
                        LINK-SAKU-07
                        LINK-SAKU-08
                        LINK-SAKU-09
                        LINK-SAKU-10
                        LINK-SAKU-11
                        LINK-SAKU-12
                        LINK-SAKU-13
                        LINK-SAKU-14
                        LINK-SAKU-15
                        LINK-SAKU-16
                        LINK-SAKU-17
                        LINK-SAKU-18
                        LINK-SAKU-19
                        LINK-SAKU-20.
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
*
 PARA-CHK-01.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
     MOVE     ZERO               TO   ERR-FLG.
 PARA-CHK-02.
*管理番号チェック
     IF       DSP-KANRNO NOT NUMERIC
         OR   DSP-KANRNO =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KANRNO
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KANRNO
              GO             TO   PARA-CHK-EXIT
     END-IF.
*
 PARA-CHK-03.
*作場コードチェック
***  未入力時，作場コード＝０
*    IF   DSP-SKBCD = SPACE
*         IF  DSP-KANRNO(1:1)    =    9
*             MOVE     SPACE      TO   DSP-SKBCD
*         ELSE
*             MOVE   6     TO   ERR-FLG
*             MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SKBCD
*             MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SKBCD
*             GO           TO   PARA-CHK-EXIT
*         END-IF
*    ELSE
*        IF   DSP-SKBCD   =   "00"
*          IF DSP-KANRNO(1:1)    =    9
*             CONTINUE
*          ELSE
*             MOVE   6     TO   ERR-FLG
*             MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SKBCD
*             MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SKBCD
*             GO           TO   PARA-CHK-EXIT
*          END-IF
*        ELSE
***  作場マスタＲＥＡＤ
*             MOVE      DSP-SKBCD     TO   SAK-F01
*             READ      SAKUBAF
*             INVALID
*                    MOVE   5     TO   ERR-FLG
*                    MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SKBCD
*                    MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SKBCD
*                    GO           TO   PARA-CHK-EXIT
*             NOT INVALID
*                    MOVE  SAK-F02     TO               DSP-SKBMEI
*                    MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SKBCD
*                    MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SKBCD
*             END-READ
*          END-IF
*        END-IF
*    END-IF.
***  本発以外は作場コード必須
     IF  DSP-KANRNO(1:1) NOT = 9
         IF ( DSP-SKB001     = SPACE OR "00" ) AND
            ( DSP-SKB002     = SPACE OR "00" ) AND
            ( DSP-SKB003     = SPACE OR "00" ) AND
            ( DSP-SKB004     = SPACE OR "00" ) AND
            ( DSP-SKB005     = SPACE OR "00" ) AND
            ( DSP-SKB006     = SPACE OR "00" ) AND
            ( DSP-SKB007     = SPACE OR "00" ) AND
            ( DSP-SKB008     = SPACE OR "00" ) AND
            ( DSP-SKB009     = SPACE OR "00" ) AND
            ( DSP-SKB010     = SPACE OR "00" ) AND
            ( DSP-SKB011     = SPACE OR "00" ) AND
            ( DSP-SKB012     = SPACE OR "00" ) AND
            ( DSP-SKB013     = SPACE OR "00" ) AND
            ( DSP-SKB014     = SPACE OR "00" ) AND
            ( DSP-SKB015     = SPACE OR "00" ) AND
            ( DSP-SKB016     = SPACE OR "00" ) AND
            ( DSP-SKB017     = SPACE OR "00" ) AND
            ( DSP-SKB018     = SPACE OR "00" ) AND
            ( DSP-SKB019     = SPACE OR "00" ) AND
            ( DSP-SKB020     = SPACE OR "00" )
              MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB001
              MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB001
              MOVE   6   TO  ERR-FLG
              GO         TO  PARA-CHK-EXIT
     END-IF.
 PARA-CHK-04.
***  本発は作場入力しても無視
     IF  DSP-KANRNO(1:1)    =    9
         MOVE      SPACE         TO   DSP-SKB001
                                      DSP-SKB002
                                      DSP-SKB003
                                      DSP-SKB004
                                      DSP-SKB005
                                      DSP-SKB006
                                      DSP-SKB007
                                      DSP-SKB008
                                      DSP-SKB009
                                      DSP-SKB010
                                      DSP-SKB011
                                      DSP-SKB012
                                      DSP-SKB013
                                      DSP-SKB014
                                      DSP-SKB015
                                      DSP-SKB016
                                      DSP-SKB017
                                      DSP-SKB018
                                      DSP-SKB019
                                      DSP-SKB020
     END-IF.
*
 PARA-CHK-05.
*1
     IF       DSP-SKB001 NOT = SPACE
              MOVE       DSP-SKB001 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB001
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB001
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB001
                   END-IF
              ELSE
                         MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB001
                         MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB001
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB001
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB001
     END-IF.
*2
     IF       DSP-SKB002 NOT = SPACE
              MOVE       DSP-SKB002 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB002
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB002
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB002
                   END-IF
              ELSE
                         MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB002
                         MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB002
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB002
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB002
     END-IF.
*3
     IF       DSP-SKB003 NOT = SPACE
              MOVE       DSP-SKB003 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB003
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB003
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB003
                   END-IF
              ELSE
                         MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB003
                         MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB003
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB003
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB003
     END-IF.
*4
     IF       DSP-SKB004 NOT = SPACE
              MOVE       DSP-SKB004 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB004
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB004
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB004
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB004
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB004
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB004
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB004
     END-IF.
*5
     IF       DSP-SKB005 NOT = SPACE
              MOVE       DSP-SKB005 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB005
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB005
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB005
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB005
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB005
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB005
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB005
     END-IF.
*6
     IF       DSP-SKB006 NOT = SPACE
              MOVE       DSP-SKB006 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB006
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB006
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB006
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB006
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB006
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB006
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB006
     END-IF.
*7
     IF       DSP-SKB007 NOT = SPACE
              MOVE       DSP-SKB007 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB007
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB007
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB007
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB007
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB007
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB007
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB007
     END-IF.
*8
     IF       DSP-SKB008 NOT = SPACE
              MOVE       DSP-SKB008 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB008
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB008
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB008
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB008
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB008
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB008
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB008
     END-IF.
*9
     IF       DSP-SKB009 NOT = SPACE
              MOVE       DSP-SKB009 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB009
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB009
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB009
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB009
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB009
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB009
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB009
     END-IF.
*10
     IF       DSP-SKB010 NOT = SPACE
              MOVE       DSP-SKB010 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB010
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB010
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB010
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB010
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB010
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB010
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB010
     END-IF.
*11
     IF       DSP-SKB011 NOT = SPACE
              MOVE       DSP-SKB011 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB011
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB011
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB011
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB011
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB011
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB011
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB011
     END-IF.
*12
     IF       DSP-SKB012 NOT = SPACE
              MOVE       DSP-SKB012 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB012
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB012
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB012
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB012
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB012
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB012
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB012
     END-IF.
*13
     IF       DSP-SKB013 NOT = SPACE
              MOVE       DSP-SKB013 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB013
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB013
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB013
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB013
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB013
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB013
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB013
     END-IF.
*14
     IF       DSP-SKB014 NOT = SPACE
              MOVE       DSP-SKB014 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB014
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB014
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB014
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB014
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB014
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB014
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB014
     END-IF.
*15
     IF       DSP-SKB015 NOT = SPACE
              MOVE       DSP-SKB015 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB015
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB015
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB015
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB015
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB015
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB015
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB015
     END-IF.
*16
     IF       DSP-SKB016 NOT = SPACE
              MOVE       DSP-SKB016 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB016
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB016
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB016
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB016
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB016
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB016
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB016
     END-IF.
*17
     IF       DSP-SKB017 NOT = SPACE
              MOVE       DSP-SKB017 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB017
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB017
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB017
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB017
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB017
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB017
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB017
     END-IF.
*18
     IF       DSP-SKB018 NOT = SPACE
              MOVE       DSP-SKB018 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB018
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB018
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB018
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB018
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB018
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB018
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB018
     END-IF.
*19
     IF       DSP-SKB019 NOT = SPACE
              MOVE       DSP-SKB019 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB019
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB019
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB019
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB019
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB019
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB019
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB019
     END-IF.
*20
     IF       DSP-SKB020 NOT = SPACE
              MOVE       DSP-SKB020 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF  ( ERR-FLG = ZERO ) OR ( ERR-FLG NOT = 5 )
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB020
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB020
                         MOVE   5   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB020
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB020
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB020
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB020
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB020
     END-IF.
*
     IF       ERR-FLG  NOT = ZERO
              GO         TO  PARA-CHK-EXIT
     END-IF.
*
     MOVE     0              TO   IX.
*
 PARA-CHK-06.
*箱数確定ファイル存在／未抽出チェック
     MOVE     SPACE          TO   WK-SKBCD.
     ADD      1              TO   IX.
     IF  IX     >  20
         GO     TO  PARA-CHK-08
     END-IF.
*
     EVALUATE IX
       WHEN 1   IF DSP-SKB001 NOT = SPACE
                   MOVE DSP-SKB001 TO WK-SKBCD  END-IF
       WHEN 2   IF DSP-SKB002 NOT = SPACE
                   MOVE DSP-SKB002 TO WK-SKBCD  END-IF
       WHEN 3   IF DSP-SKB003 NOT = SPACE
                   MOVE DSP-SKB003 TO WK-SKBCD  END-IF
       WHEN 4   IF DSP-SKB004 NOT = SPACE
                   MOVE DSP-SKB004 TO WK-SKBCD  END-IF
       WHEN 5   IF DSP-SKB005 NOT = SPACE
                   MOVE DSP-SKB005 TO WK-SKBCD  END-IF
       WHEN 6   IF DSP-SKB006 NOT = SPACE
                   MOVE DSP-SKB006 TO WK-SKBCD  END-IF
       WHEN 7   IF DSP-SKB007 NOT = SPACE
                   MOVE DSP-SKB007 TO WK-SKBCD  END-IF
       WHEN 8   IF DSP-SKB008 NOT = SPACE
                   MOVE DSP-SKB008 TO WK-SKBCD  END-IF
       WHEN 9   IF DSP-SKB009 NOT = SPACE
                   MOVE DSP-SKB009 TO WK-SKBCD  END-IF
       WHEN 10  IF DSP-SKB010 NOT = SPACE
                   MOVE DSP-SKB010 TO WK-SKBCD  END-IF
       WHEN 11  IF DSP-SKB011 NOT = SPACE
                   MOVE DSP-SKB011 TO WK-SKBCD  END-IF
       WHEN 12  IF DSP-SKB012 NOT = SPACE
                   MOVE DSP-SKB012 TO WK-SKBCD  END-IF
       WHEN 13  IF DSP-SKB013 NOT = SPACE
                   MOVE DSP-SKB013 TO WK-SKBCD  END-IF
       WHEN 14  IF DSP-SKB014 NOT = SPACE
                   MOVE DSP-SKB014 TO WK-SKBCD  END-IF
       WHEN 15  IF DSP-SKB015 NOT = SPACE
                   MOVE DSP-SKB015 TO WK-SKBCD  END-IF
       WHEN 16  IF DSP-SKB016 NOT = SPACE
                   MOVE DSP-SKB016 TO WK-SKBCD  END-IF
       WHEN 17  IF DSP-SKB017 NOT = SPACE
                   MOVE DSP-SKB017 TO WK-SKBCD  END-IF
       WHEN 18  IF DSP-SKB018 NOT = SPACE
                   MOVE DSP-SKB018 TO WK-SKBCD  END-IF
       WHEN 19  IF DSP-SKB019 NOT = SPACE
                   MOVE DSP-SKB019 TO WK-SKBCD  END-IF
       WHEN 20  IF DSP-SKB020 NOT = SPACE
                   MOVE DSP-SKB020 TO WK-SKBCD  END-IF
     END-EVALUATE.
*
     IF  WK-SKBCD = SPACE
         GO     TO  PARA-CHK-06
     END-IF.
*
 PARA-CHK-07.
*スタート
     MOVE     SPACE          TO   HK1-REC.
     INITIALIZE                   HK1-REC.
     MOVE     DSP-KANRNO     TO   HK1-F01.
     MOVE     WK-SKBCD       TO   HK1-F05.
     START    NFHAKOF   KEY  IS   >=   HK1-F01  HK1-F05  HK1-F06
                                       HK1-F07  HK1-F08
     INVALID
              MOVE      4    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-KANRNO
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-KANRNO
              IF  DSP-KANRNO(1:1) NOT = 9
                  EVALUATE IX
                    WHEN 1   MOVE "R" TO EDIT-OPTION OF DSP-SKB001
                    WHEN 2   MOVE "R" TO EDIT-OPTION OF DSP-SKB002
                    WHEN 3   MOVE "R" TO EDIT-OPTION OF DSP-SKB003
                    WHEN 4   MOVE "R" TO EDIT-OPTION OF DSP-SKB004
                    WHEN 5   MOVE "R" TO EDIT-OPTION OF DSP-SKB005
                    WHEN 6   MOVE "R" TO EDIT-OPTION OF DSP-SKB006
                    WHEN 7   MOVE "R" TO EDIT-OPTION OF DSP-SKB007
                    WHEN 8   MOVE "R" TO EDIT-OPTION OF DSP-SKB008
                    WHEN 9   MOVE "R" TO EDIT-OPTION OF DSP-SKB009
                    WHEN 10  MOVE "R" TO EDIT-OPTION OF DSP-SKB010
                    WHEN 11  MOVE "R" TO EDIT-OPTION OF DSP-SKB011
                    WHEN 12  MOVE "R" TO EDIT-OPTION OF DSP-SKB012
                    WHEN 13  MOVE "R" TO EDIT-OPTION OF DSP-SKB013
                    WHEN 14  MOVE "R" TO EDIT-OPTION OF DSP-SKB014
                    WHEN 15  MOVE "R" TO EDIT-OPTION OF DSP-SKB015
                    WHEN 16  MOVE "R" TO EDIT-OPTION OF DSP-SKB016
                    WHEN 17  MOVE "R" TO EDIT-OPTION OF DSP-SKB017
                    WHEN 18  MOVE "R" TO EDIT-OPTION OF DSP-SKB018
                    WHEN 19  MOVE "R" TO EDIT-OPTION OF DSP-SKB019
                    WHEN 20  MOVE "R" TO EDIT-OPTION OF DSP-SKB020
                  END-EVALUATE
              END-IF
              GO             TO   PARA-CHK-EXIT
     NOT INVALID
***           箱数確定ファイル読込
              READ     NFHAKOF
              END-READ
***           管理番号が画面と同一かチェック
              IF   DSP-KANRNO  =  HK1-F01
                   IF  DSP-KANRNO(1:1) = 9
*                      IF  HK1-F98 =  SPACE
*                          MOVE SPACE TO EDIT-CURSOR OF DSP-KANRNO
*                      ELSE
*                          MOVE   2  TO ERR-FLG
*                          MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
*                          MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
*                          GO        TO PARA-CHK-EXIT
*                      END-IF
                       IF  HK1-F93 =  "1"
                           MOVE SPACE TO EDIT-CURSOR OF DSP-KANRNO
                       ELSE
                           MOVE   7  TO ERR-FLG
                           MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
                           MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
                           GO        TO PARA-CHK-EXIT
                       END-IF
                   ELSE
                       IF  HK1-F05 =  WK-SKBCD
*                          IF  HK1-F98 =  SPACE
*                              MOVE SPACE TO EDIT-CURSOR
*                                                   OF DSP-KANRNO
*                          ELSE
*                              MOVE   2  TO ERR-FLG
*                              MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-KANRNO
*                              EVALUATE IX
*                               WHEN 1
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB001
*                               WHEN 2
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB002
*                               WHEN 3
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB003
*                               WHEN 4
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB004
*                               WHEN 5
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB005
*                               WHEN 6
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB006
*                               WHEN 7
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB007
*                               WHEN 8
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB008
*                               WHEN 9
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB009
*                               WHEN 10
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB010
*                               WHEN 11
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB011
*                               WHEN 12
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB012
*                               WHEN 13
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB013
*                               WHEN 14
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB014
*                               WHEN 15
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB015
*                               WHEN 16
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB016
*                               WHEN 17
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB017
*                               WHEN 18
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB018
*                               WHEN 19
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB019
*                               WHEN 20
*                                 MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-SKB020
*                              END-EVALUATE
*                              MOVE  "C" TO EDIT-CURSOR
*                                                   OF DSP-KANRNO
*                              GO        TO PARA-CHK-EXIT
*                          END-IF
                           IF  HK1-F93 =  "1"
                               MOVE SPACE TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                           ELSE
                               MOVE   7  TO ERR-FLG
                               MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-KANRNO
                               EVALUATE IX
                                WHEN 1
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB001
                                WHEN 2
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB002
                                WHEN 3
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB003
                                WHEN 4
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB004
                                WHEN 5
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB005
                                WHEN 6
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB006
                                WHEN 7
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB007
                                WHEN 8
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB008
                                WHEN 9
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB009
                                WHEN 10
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB010
                                WHEN 11
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB011
                                WHEN 12
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB012
                                WHEN 13
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB013
                                WHEN 14
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB014
                                WHEN 15
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB015
                                WHEN 16
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB016
                                WHEN 17
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB017
                                WHEN 18
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB018
                                WHEN 19
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB019
                                WHEN 20
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB020
                               MOVE  "C" TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                               END-EVALUATE
                               GO        TO PARA-CHK-EXIT
                           END-IF
                       ELSE
                           MOVE   4  TO ERR-FLG
                           MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
                           EVALUATE IX
                             WHEN 1
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB001
                             WHEN 2
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB002
                             WHEN 3
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB003
                             WHEN 4
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB004
                             WHEN 5
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB005
                             WHEN 6
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB006
                             WHEN 7
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB007
                             WHEN 8
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB008
                             WHEN 9
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB009
                             WHEN 10
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB010
                             WHEN 11
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB011
                             WHEN 12
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB012
                             WHEN 13
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB013
                             WHEN 14
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB014
                             WHEN 15
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB015
                             WHEN 16
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB016
                             WHEN 17
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB017
                             WHEN 18
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB018
                             WHEN 19
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB019
                             WHEN 20
                                 MOVE "R" TO EDIT-OPTION
                                                  OF DSP-SKB020
                           END-EVALUATE
                           MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
                           GO        TO PARA-CHK-EXIT
                       END-IF
                   END-IF
              ELSE
                   MOVE   4       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION OF DSP-KANRNO
                   MOVE  "C"      TO   EDIT-CURSOR OF DSP-KANRNO
                   IF  DSP-KANRNO(1:1) NOT = 9
                       EVALUATE IX
                         WHEN 1
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB001
                         WHEN 2
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB002
                         WHEN 3
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB003
                         WHEN 4
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB004
                         WHEN 5
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB005
                         WHEN 6
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB006
                         WHEN 7
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB007
                         WHEN 8
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB008
                         WHEN 9
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB009
                         WHEN 10
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB010
                         WHEN 11
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB011
                         WHEN 12
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB012
                         WHEN 13
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB013
                         WHEN 14
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB014
                         WHEN 15
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB015
                         WHEN 16
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB016
                         WHEN 17
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB017
                         WHEN 18
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB018
                         WHEN 19
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB019
                         WHEN 20
                            MOVE "R" TO EDIT-OPTION OF DSP-SKB020
                       END-EVALUATE
                   END-IF
                   GO             TO   PARA-CHK-EXIT
              END-IF
     END-START.
*数量確定ファイル存在／未抽出チェック
*スタート
     MOVE     SPACE          TO   ST1-REC.
     INITIALIZE                   ST1-REC.
     MOVE     DSP-KANRNO     TO   ST1-F01.
     MOVE     WK-SKBCD       TO   ST1-F05.
     START    NFSUTEF   KEY  IS   >=   ST1-F01  ST1-F05  ST1-F06
                                       ST1-F07  ST1-F08  ST1-F09
     INVALID
              MOVE      4    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-KANRNO
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-KANRNO
              IF  DSP-KANRNO(1:1) NOT = 9
                  EVALUATE IX
                    WHEN 1
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB001
                    WHEN 2
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB002
                    WHEN 3
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB003
                    WHEN 4
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB004
                    WHEN 5
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB005
                    WHEN 6
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB006
                    WHEN 7
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB007
                    WHEN 8
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB008
                    WHEN 9
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB009
                    WHEN 10
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB010
                    WHEN 11
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB011
                    WHEN 12
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB012
                    WHEN 13
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB013
                    WHEN 14
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB014
                    WHEN 15
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB015
                    WHEN 16
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB016
                    WHEN 17
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB017
                    WHEN 18
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB018
                    WHEN 19
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB019
                    WHEN 20
                       MOVE "R"   TO   EDIT-OPTION OF DSP-SKB020
                  END-EVALUATE
              END-IF
              GO             TO   PARA-CHK-EXIT
     NOT INVALID
***           数量確定ファイル読込
              READ     NFSUTEF
              END-READ
***           管理番号が画面と同一かチェック
              IF   DSP-KANRNO  =  ST1-F01
                   IF  DSP-KANRNO(1:1) = 9
*                      IF  ST1-F98 =  SPACE
*                          MOVE SPACE TO EDIT-CURSOR OF DSP-KANRNO
*                      ELSE
*                          MOVE   2  TO ERR-FLG
*                          MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
*                          MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
*                          GO        TO PARA-CHK-EXIT
*                      END-IF
                       IF  ST1-F91 =  "1"
                           MOVE SPACE TO EDIT-CURSOR OF DSP-KANRNO
                       ELSE
                           MOVE   7  TO ERR-FLG
                           MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
                           MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
                           GO        TO PARA-CHK-EXIT
                       END-IF
                   ELSE
                       IF  ST1-F05 =  WK-SKBCD
*                          IF  ST1-F98 =  SPACE
*                              MOVE SPACE TO EDIT-CURSOR
*                                                   OF DSP-KANRNO
*                          ELSE
*                              MOVE   2  TO ERR-FLG
*                              MOVE  "R" TO EDIT-OPTION
*                                                   OF DSP-KANRNO
*                              EVALUATE IX
*                                WHEN 1
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB001
*                                WHEN 2
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB002
*                                WHEN 3
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB003
*                                WHEN 4
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB004
*                                WHEN 5
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB005
*                                WHEN 6
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB006
*                                WHEN 7
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB007
*                                WHEN 8
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB008
*                                WHEN 9
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB009
*                                WHEN 10
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB010
*                                WHEN 11
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB011
*                                WHEN 12
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB012
*                                WHEN 13
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB013
*                                WHEN 14
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB014
*                                WHEN 15
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB015
*                                WHEN 16
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB016
*                                WHEN 17
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB017
*                                WHEN 18
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB018
*                                WHEN 19
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB019
*                                WHEN 20
*                                  MOVE "R" TO EDIT-OPTION
*                                                   OF DSP-SKB020
*                              END-EVALUATE
*                              MOVE  "C" TO EDIT-CURSOR
*                                                   OF DSP-KANRNO
*                              GO        TO PARA-CHK-EXIT
*                          END-IF
                           IF  ST1-F91 =  "1"
                               MOVE SPACE TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                           ELSE
                               MOVE   7  TO ERR-FLG
                               MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-KANRNO
                               EVALUATE IX
                                 WHEN 1
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB001
                                 WHEN 2
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB002
                                 WHEN 3
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB003
                                 WHEN 4
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB004
                                 WHEN 5
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB005
                                 WHEN 6
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB006
                                 WHEN 7
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB007
                                 WHEN 8
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB008
                                 WHEN 9
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB009
                                 WHEN 10
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB010
                                 WHEN 11
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB011
                                 WHEN 12
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB012
                                 WHEN 13
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB013
                                 WHEN 14
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB014
                                 WHEN 15
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB015
                                 WHEN 16
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB016
                                 WHEN 17
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB017
                                 WHEN 18
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB018
                                 WHEN 19
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB019
                                 WHEN 20
                                  MOVE  "R" TO EDIT-OPTION
                                                    OF DSP-SKB020
                               END-EVALUATE
                               MOVE  "C" TO EDIT-CURSOR
                                                    OF DSP-KANRNO
                               GO        TO PARA-CHK-EXIT
                           END-IF
                       ELSE
                           MOVE   4  TO ERR-FLG
                           MOVE  "R" TO EDIT-OPTION OF DSP-KANRNO
                           EVALUATE IX
                             WHEN 1
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB001
                             WHEN 2
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB002
                             WHEN 3
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB003
                             WHEN 4
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB004
                             WHEN 5
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB005
                             WHEN 6
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB006
                             WHEN 7
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB007
                             WHEN 8
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB008
                             WHEN 9
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB009
                             WHEN 10
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB010
                             WHEN 11
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB011
                             WHEN 12
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB012
                             WHEN 13
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB013
                             WHEN 14
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB014
                             WHEN 15
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB015
                             WHEN 16
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB016
                             WHEN 17
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB017
                             WHEN 18
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB018
                             WHEN 19
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB019
                             WHEN 20
                               MOVE  "R" TO EDIT-OPTION
                                                 OF DSP-SKB020
                           END-EVALUATE
                           MOVE  "C" TO EDIT-CURSOR OF DSP-KANRNO
                           GO        TO PARA-CHK-EXIT
                       END-IF
                   END-IF
              ELSE
                   MOVE   4       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION OF DSP-KANRNO
                   MOVE  "C"      TO   EDIT-CURSOR OF DSP-KANRNO
                   IF  DSP-KANRNO(1:1) NOT = 9
                       EVALUATE IX
                         WHEN 1
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB001
                         WHEN 2
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB002
                         WHEN 3
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB003
                         WHEN 4
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB004
                         WHEN 5
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB005
                         WHEN 6
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB006
                         WHEN 7
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB007
                         WHEN 8
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB008
                         WHEN 9
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB009
                         WHEN 10
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB010
                         WHEN 11
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB011
                         WHEN 12
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB012
                         WHEN 13
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB013
                         WHEN 14
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB014
                         WHEN 15
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB015
                         WHEN 16
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB016
                         WHEN 17
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB017
                         WHEN 18
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB018
                         WHEN 19
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB019
                         WHEN 20
                           MOVE "R" TO EDIT-OPTION OF DSP-SKB020
                       END-EVALUATE
                   END-IF
                   GO             TO   PARA-CHK-EXIT
              END-IF
     END-START.
     GO     TO  PARA-CHK-06.
*
 PARA-CHK-08.
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
****            MOVE    DSP-SKBCD     TO   LINK-SAKUBA
                MOVE    SPACE         TO   WK-SAKU
                MOVE    1             TO   IX
                IF      DSP-SKB001 NOT =   SPACE
                        MOVE  DSP-SKB001   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                END-IF
                IF      DSP-SKB002 NOT =   SPACE
                    IF  DSP-SKB002 NOT =   DSP-SKB001
                        MOVE  DSP-SKB002   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB003 NOT =   SPACE
                    IF  DSP-SKB003 NOT =   DSP-SKB001 AND
                        DSP-SKB003 NOT =   DSP-SKB002
                        MOVE  DSP-SKB003   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB004 NOT =   SPACE
                    IF  DSP-SKB004 NOT =   DSP-SKB001 AND
                        DSP-SKB004 NOT =   DSP-SKB002 AND
                        DSP-SKB004 NOT =   DSP-SKB003
                        MOVE  DSP-SKB004   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB005 NOT =   SPACE
                    IF  DSP-SKB005 NOT =   DSP-SKB001 AND
                        DSP-SKB005 NOT =   DSP-SKB002 AND
                        DSP-SKB005 NOT =   DSP-SKB003 AND
                        DSP-SKB005 NOT =   DSP-SKB004
                        MOVE  DSP-SKB005   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB006 NOT =   SPACE
                    IF  DSP-SKB006 NOT =   DSP-SKB001 AND
                        DSP-SKB006 NOT =   DSP-SKB002 AND
                        DSP-SKB006 NOT =   DSP-SKB003 AND
                        DSP-SKB006 NOT =   DSP-SKB004 AND
                        DSP-SKB006 NOT =   DSP-SKB005
                        MOVE  DSP-SKB006   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB007 NOT =   SPACE
                    IF  DSP-SKB007 NOT =   DSP-SKB001 AND
                        DSP-SKB007 NOT =   DSP-SKB002 AND
                        DSP-SKB007 NOT =   DSP-SKB003 AND
                        DSP-SKB007 NOT =   DSP-SKB004 AND
                        DSP-SKB007 NOT =   DSP-SKB005 AND
                        DSP-SKB007 NOT =   DSP-SKB006
                        MOVE  DSP-SKB007   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB008 NOT =   SPACE
                    IF  DSP-SKB008 NOT =   DSP-SKB001 AND
                        DSP-SKB008 NOT =   DSP-SKB002 AND
                        DSP-SKB008 NOT =   DSP-SKB003 AND
                        DSP-SKB008 NOT =   DSP-SKB004 AND
                        DSP-SKB008 NOT =   DSP-SKB005 AND
                        DSP-SKB008 NOT =   DSP-SKB006 AND
                        DSP-SKB008 NOT =   DSP-SKB007
                        MOVE  DSP-SKB008   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB009 NOT =   SPACE
                    IF  DSP-SKB009 NOT =   DSP-SKB001 AND
                        DSP-SKB009 NOT =   DSP-SKB002 AND
                        DSP-SKB009 NOT =   DSP-SKB003 AND
                        DSP-SKB009 NOT =   DSP-SKB004 AND
                        DSP-SKB009 NOT =   DSP-SKB005 AND
                        DSP-SKB009 NOT =   DSP-SKB006 AND
                        DSP-SKB009 NOT =   DSP-SKB007 AND
                        DSP-SKB009 NOT =   DSP-SKB008
                        MOVE  DSP-SKB009   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB010 NOT =   SPACE
                    IF  DSP-SKB010 NOT =   DSP-SKB001 AND
                        DSP-SKB010 NOT =   DSP-SKB002 AND
                        DSP-SKB010 NOT =   DSP-SKB003 AND
                        DSP-SKB010 NOT =   DSP-SKB004 AND
                        DSP-SKB010 NOT =   DSP-SKB005 AND
                        DSP-SKB010 NOT =   DSP-SKB006 AND
                        DSP-SKB010 NOT =   DSP-SKB007 AND
                        DSP-SKB010 NOT =   DSP-SKB008 AND
                        DSP-SKB010 NOT =   DSP-SKB009
                        MOVE  DSP-SKB010   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB011 NOT =   SPACE
                    IF  DSP-SKB011 NOT =   DSP-SKB001 AND
                        DSP-SKB011 NOT =   DSP-SKB002 AND
                        DSP-SKB011 NOT =   DSP-SKB003 AND
                        DSP-SKB011 NOT =   DSP-SKB004 AND
                        DSP-SKB011 NOT =   DSP-SKB005 AND
                        DSP-SKB011 NOT =   DSP-SKB006 AND
                        DSP-SKB011 NOT =   DSP-SKB007 AND
                        DSP-SKB011 NOT =   DSP-SKB008 AND
                        DSP-SKB011 NOT =   DSP-SKB009 AND
                        DSP-SKB011 NOT =   DSP-SKB010
                        MOVE  DSP-SKB011   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB012 NOT =   SPACE
                    IF  DSP-SKB012 NOT =   DSP-SKB001 AND
                        DSP-SKB012 NOT =   DSP-SKB002 AND
                        DSP-SKB012 NOT =   DSP-SKB003 AND
                        DSP-SKB012 NOT =   DSP-SKB004 AND
                        DSP-SKB012 NOT =   DSP-SKB005 AND
                        DSP-SKB012 NOT =   DSP-SKB006 AND
                        DSP-SKB012 NOT =   DSP-SKB007 AND
                        DSP-SKB012 NOT =   DSP-SKB008 AND
                        DSP-SKB012 NOT =   DSP-SKB009 AND
                        DSP-SKB012 NOT =   DSP-SKB010 AND
                        DSP-SKB012 NOT =   DSP-SKB011
                        MOVE  DSP-SKB012   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB013 NOT =   SPACE
                    IF  DSP-SKB013 NOT =   DSP-SKB001 AND
                        DSP-SKB013 NOT =   DSP-SKB002 AND
                        DSP-SKB013 NOT =   DSP-SKB003 AND
                        DSP-SKB013 NOT =   DSP-SKB004 AND
                        DSP-SKB013 NOT =   DSP-SKB005 AND
                        DSP-SKB013 NOT =   DSP-SKB006 AND
                        DSP-SKB013 NOT =   DSP-SKB007 AND
                        DSP-SKB013 NOT =   DSP-SKB008 AND
                        DSP-SKB013 NOT =   DSP-SKB009 AND
                        DSP-SKB013 NOT =   DSP-SKB010 AND
                        DSP-SKB013 NOT =   DSP-SKB011 AND
                        DSP-SKB013 NOT =   DSP-SKB012
                        MOVE  DSP-SKB013   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB014 NOT =   SPACE
                    IF  DSP-SKB014 NOT =   DSP-SKB001 AND
                        DSP-SKB014 NOT =   DSP-SKB002 AND
                        DSP-SKB014 NOT =   DSP-SKB003 AND
                        DSP-SKB014 NOT =   DSP-SKB004 AND
                        DSP-SKB014 NOT =   DSP-SKB005 AND
                        DSP-SKB014 NOT =   DSP-SKB006 AND
                        DSP-SKB014 NOT =   DSP-SKB007 AND
                        DSP-SKB014 NOT =   DSP-SKB008 AND
                        DSP-SKB014 NOT =   DSP-SKB009 AND
                        DSP-SKB014 NOT =   DSP-SKB010 AND
                        DSP-SKB014 NOT =   DSP-SKB011 AND
                        DSP-SKB014 NOT =   DSP-SKB012 AND
                        DSP-SKB014 NOT =   DSP-SKB013
                        MOVE  DSP-SKB014   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB015 NOT =   SPACE
                    IF  DSP-SKB015 NOT =   DSP-SKB001 AND
                        DSP-SKB015 NOT =   DSP-SKB002 AND
                        DSP-SKB015 NOT =   DSP-SKB003 AND
                        DSP-SKB015 NOT =   DSP-SKB004 AND
                        DSP-SKB015 NOT =   DSP-SKB005 AND
                        DSP-SKB015 NOT =   DSP-SKB006 AND
                        DSP-SKB015 NOT =   DSP-SKB007 AND
                        DSP-SKB015 NOT =   DSP-SKB008 AND
                        DSP-SKB015 NOT =   DSP-SKB009 AND
                        DSP-SKB015 NOT =   DSP-SKB010 AND
                        DSP-SKB015 NOT =   DSP-SKB011 AND
                        DSP-SKB015 NOT =   DSP-SKB012 AND
                        DSP-SKB015 NOT =   DSP-SKB013 AND
                        DSP-SKB015 NOT =   DSP-SKB014
                        MOVE  DSP-SKB015   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB016 NOT =   SPACE
                    IF  DSP-SKB016 NOT =   DSP-SKB001 AND
                        DSP-SKB016 NOT =   DSP-SKB002 AND
                        DSP-SKB016 NOT =   DSP-SKB003 AND
                        DSP-SKB016 NOT =   DSP-SKB004 AND
                        DSP-SKB016 NOT =   DSP-SKB005 AND
                        DSP-SKB016 NOT =   DSP-SKB006 AND
                        DSP-SKB016 NOT =   DSP-SKB007 AND
                        DSP-SKB016 NOT =   DSP-SKB008 AND
                        DSP-SKB016 NOT =   DSP-SKB009 AND
                        DSP-SKB016 NOT =   DSP-SKB010 AND
                        DSP-SKB016 NOT =   DSP-SKB011 AND
                        DSP-SKB016 NOT =   DSP-SKB012 AND
                        DSP-SKB016 NOT =   DSP-SKB013 AND
                        DSP-SKB016 NOT =   DSP-SKB014 AND
                        DSP-SKB016 NOT =   DSP-SKB015
                        MOVE  DSP-SKB016   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB017 NOT =   SPACE
                    IF  DSP-SKB017 NOT =   DSP-SKB001 AND
                        DSP-SKB017 NOT =   DSP-SKB002 AND
                        DSP-SKB017 NOT =   DSP-SKB003 AND
                        DSP-SKB017 NOT =   DSP-SKB004 AND
                        DSP-SKB017 NOT =   DSP-SKB005 AND
                        DSP-SKB017 NOT =   DSP-SKB006 AND
                        DSP-SKB017 NOT =   DSP-SKB007 AND
                        DSP-SKB017 NOT =   DSP-SKB008 AND
                        DSP-SKB017 NOT =   DSP-SKB009 AND
                        DSP-SKB017 NOT =   DSP-SKB010 AND
                        DSP-SKB017 NOT =   DSP-SKB011 AND
                        DSP-SKB017 NOT =   DSP-SKB012 AND
                        DSP-SKB017 NOT =   DSP-SKB013 AND
                        DSP-SKB017 NOT =   DSP-SKB014 AND
                        DSP-SKB017 NOT =   DSP-SKB015 AND
                        DSP-SKB017 NOT =   DSP-SKB016
                        MOVE  DSP-SKB017   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB018 NOT =   SPACE
                    IF  DSP-SKB018 NOT =   DSP-SKB001 AND
                        DSP-SKB018 NOT =   DSP-SKB002 AND
                        DSP-SKB018 NOT =   DSP-SKB003 AND
                        DSP-SKB018 NOT =   DSP-SKB004 AND
                        DSP-SKB018 NOT =   DSP-SKB005 AND
                        DSP-SKB018 NOT =   DSP-SKB006 AND
                        DSP-SKB018 NOT =   DSP-SKB007 AND
                        DSP-SKB018 NOT =   DSP-SKB008 AND
                        DSP-SKB018 NOT =   DSP-SKB009 AND
                        DSP-SKB018 NOT =   DSP-SKB010 AND
                        DSP-SKB018 NOT =   DSP-SKB011 AND
                        DSP-SKB018 NOT =   DSP-SKB012 AND
                        DSP-SKB018 NOT =   DSP-SKB013 AND
                        DSP-SKB018 NOT =   DSP-SKB014 AND
                        DSP-SKB018 NOT =   DSP-SKB015 AND
                        DSP-SKB018 NOT =   DSP-SKB016 AND
                        DSP-SKB018 NOT =   DSP-SKB017
                        MOVE  DSP-SKB018   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB019 NOT =   SPACE
                    IF  DSP-SKB019 NOT =   DSP-SKB001 AND
                        DSP-SKB019 NOT =   DSP-SKB002 AND
                        DSP-SKB019 NOT =   DSP-SKB003 AND
                        DSP-SKB019 NOT =   DSP-SKB004 AND
                        DSP-SKB019 NOT =   DSP-SKB005 AND
                        DSP-SKB019 NOT =   DSP-SKB006 AND
                        DSP-SKB019 NOT =   DSP-SKB007 AND
                        DSP-SKB019 NOT =   DSP-SKB008 AND
                        DSP-SKB019 NOT =   DSP-SKB009 AND
                        DSP-SKB019 NOT =   DSP-SKB010 AND
                        DSP-SKB019 NOT =   DSP-SKB011 AND
                        DSP-SKB019 NOT =   DSP-SKB012 AND
                        DSP-SKB019 NOT =   DSP-SKB013 AND
                        DSP-SKB019 NOT =   DSP-SKB014 AND
                        DSP-SKB019 NOT =   DSP-SKB015 AND
                        DSP-SKB019 NOT =   DSP-SKB016 AND
                        DSP-SKB019 NOT =   DSP-SKB017 AND
                        DSP-SKB019 NOT =   DSP-SKB018
                        MOVE  DSP-SKB019   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB020 NOT =   SPACE
                    IF  DSP-SKB020 NOT =   DSP-SKB001 AND
                        DSP-SKB020 NOT =   DSP-SKB002 AND
                        DSP-SKB020 NOT =   DSP-SKB003 AND
                        DSP-SKB020 NOT =   DSP-SKB004 AND
                        DSP-SKB020 NOT =   DSP-SKB005 AND
                        DSP-SKB020 NOT =   DSP-SKB006 AND
                        DSP-SKB020 NOT =   DSP-SKB007 AND
                        DSP-SKB020 NOT =   DSP-SKB008 AND
                        DSP-SKB020 NOT =   DSP-SKB009 AND
                        DSP-SKB020 NOT =   DSP-SKB010 AND
                        DSP-SKB020 NOT =   DSP-SKB011 AND
                        DSP-SKB020 NOT =   DSP-SKB012 AND
                        DSP-SKB020 NOT =   DSP-SKB013 AND
                        DSP-SKB020 NOT =   DSP-SKB014 AND
                        DSP-SKB020 NOT =   DSP-SKB015 AND
                        DSP-SKB020 NOT =   DSP-SKB016 AND
                        DSP-SKB020 NOT =   DSP-SKB017 AND
                        DSP-SKB020 NOT =   DSP-SKB018 AND
                        DSP-SKB020 NOT =   DSP-SKB019
                        MOVE  DSP-SKB020   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                MOVE    WK-SAKU            TO   LINK-SAKUBA
*               ＯＮＬ／手書判断
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
     MOVE    "FSY39001"          TO   DSP-FMT.
     WRITE    DSP-FSY39001.
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
                MOVE    "GRP002"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY39001"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY39001.
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
     MOVE    0                    TO   ERR-FLG.
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
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKB001
                          EDIT-OPTION  OF  DSP-SKB002
                          EDIT-OPTION  OF  DSP-SKB003
                          EDIT-OPTION  OF  DSP-SKB004
                          EDIT-OPTION  OF  DSP-SKB005
                          EDIT-OPTION  OF  DSP-SKB006
                          EDIT-OPTION  OF  DSP-SKB007
                          EDIT-OPTION  OF  DSP-SKB008
                          EDIT-OPTION  OF  DSP-SKB009
                          EDIT-OPTION  OF  DSP-SKB010
                          EDIT-OPTION  OF  DSP-SKB011
                          EDIT-OPTION  OF  DSP-SKB012
                          EDIT-OPTION  OF  DSP-SKB013
                          EDIT-OPTION  OF  DSP-SKB014
                          EDIT-OPTION  OF  DSP-SKB015
                          EDIT-OPTION  OF  DSP-SKB016
                          EDIT-OPTION  OF  DSP-SKB017
                          EDIT-OPTION  OF  DSP-SKB018
                          EDIT-OPTION  OF  DSP-SKB019
                          EDIT-OPTION  OF  DSP-SKB020.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKB001
                          EDIT-CURSOR  OF  DSP-SKB002
                          EDIT-CURSOR  OF  DSP-SKB003
                          EDIT-CURSOR  OF  DSP-SKB004
                          EDIT-CURSOR  OF  DSP-SKB005
                          EDIT-CURSOR  OF  DSP-SKB006
                          EDIT-CURSOR  OF  DSP-SKB007
                          EDIT-CURSOR  OF  DSP-SKB008
                          EDIT-CURSOR  OF  DSP-SKB009
                          EDIT-CURSOR  OF  DSP-SKB010
                          EDIT-CURSOR  OF  DSP-SKB011
                          EDIT-CURSOR  OF  DSP-SKB012
                          EDIT-CURSOR  OF  DSP-SKB013
                          EDIT-CURSOR  OF  DSP-SKB014
                          EDIT-CURSOR  OF  DSP-SKB015
                          EDIT-CURSOR  OF  DSP-SKB016
                          EDIT-CURSOR  OF  DSP-SKB017
                          EDIT-CURSOR  OF  DSP-SKB018
                          EDIT-CURSOR  OF  DSP-SKB019
                          EDIT-CURSOR  OF  DSP-SKB020.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             作場マスタ索引                                   *
****************************************************************
 SAKUBAF-READ-SEC     SECTION.
     MOVE     "SAKUBAF-READ-SEC" TO   S-NAME.
*
     READ      SAKUBAF
        INVALID
               MOVE    "INV"     TO   SAKUBAF-INV-FLG
        NOT INVALID
               MOVE    "   "     TO   SAKUBAF-INV-FLG
     END-READ.
*
 SAKUBAF-READ-EXIT.
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
*****************<<  SSY3900I   END PROGRAM  >>******************
