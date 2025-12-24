# SSY3761I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3761I.COB`

## ソースコード

```cobol
***********************************************************
*                                                         *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　 *
*    サブシステム　　　　：　ナフコＥＤＩ受信システム　   *
*    業務名　　　　　　　：　ナフコＥＤＩ受信             *
*    モジュール名　　　　：　箱数訂正入力                 *
*    作成日／更新日　　　：　2010/10/05                   *
*    作成者／更新者　　　：　ＮＡＶ飯田                   *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　 *
*      箱数ファイルの箱数の実績により変更の発生した箱数   *
*      を訂正する。                                       *
*                                                         *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3761I.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2010/10/05.
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
*箱数ファイル
     SELECT  NFHAKOF   ASSIGN    TO        NFHAKOL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       HK1-F01
                                           HK1-F05
                                           HK1-F06
                                           HK1-F07
                                           HK1-F08
                       FILE      STATUS    HK1-ST.
*箱数ファイル２
     SELECT  NFHAKOF2  ASSIGN    TO        NFHAKOL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       HK2-F02
                                           HK2-F03
                                           HK2-F04
                                           HK2-F05
                                           HK2-F06
                                           HK2-F07
                                           HK2-F08
                       FILE      STATUS    HK2-ST.

*作場マスタ
     SELECT  SAKUBAF   ASSIGN    TO        SAKUBAL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SKB-F01
                       FILE      STATUS    SKB-ST.
*店舗マスタ
     SELECT  HTENMS    ASSIGN    TO        TENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TEN-F52
                                           TEN-F011
                       FILE      STATUS    TEN-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY37611  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
*    FILE = 箱数ファイル                                       *
****************************************************************
 FD  NFHAKOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFHAKOF   OF   XFDLIB
                       JOINING   HK1       AS   PREFIX.
****************************************************************
*    FILE = 箱数ファイル２                                     *
****************************************************************
 FD  NFHAKOF2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFHAKOF   OF   XFDLIB
                       JOINING   HK2       AS   PREFIX.

****************************************************************
*    FILE = 作場マスタ                                         *
****************************************************************
 FD  SAKUBAF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SKB       AS   PREFIX.
****************************************************************
*    FILE = 店舗マスタ                                         *
****************************************************************
 FD  HTENMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  HK1-ST                   PIC  X(02).
     03  HK2-ST                   PIC  X(02).
     03  SKB-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
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
     03  FG-SAKUBAF-INV           PIC  9(01)  VALUE  ZERO.
     03  FG-NFHAKOF-INV           PIC  9(01)  VALUE  ZERO.
     03  FG-NFHAKOF2-INV          PIC  9(01)  VALUE  ZERO.
     03  FG-NFHAKOF-END           PIC  X(03)  VALUE  SPACE.
     03  FG-NFHAKOF2-END          PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
     03  IX                       PIC  9(05).
     03  IX-PG                    PIC  9(05).
     03  WK-AMARI                 PIC  9(05).
     03  WK-RD-KANRNO             PIC  9(08).
     03  SV-HD-BCHNO1             PIC  9(08).
     03  SV-HD-BCHNO2             PIC  9(04).
     03  SV-HD-BCHNO3             PIC  9(08).
     03  SV-HD-KNRINO             PIC  9(08).
     03  SV-HD-TENYMD             PIC  9(08).
     03  SV-HD-SKBCD              PIC  X(02).


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

* 箱数ファイルデータ格納テーブル
 01  TBL-HAKO-G.
    03  TBL-HAKO-G1.
      05  TBL-KANRNO        PIC  9(08).  *> 管理番号

    03  TBL-HAKO-G2.
      05  TBL-HAKO-G3  OCCURS 1020.
        07  TBL-KAKUNO-FG   PIC  X(01).  *> テーブル格納
        07  TBL-TENCD       PIC  9(05).  *> 店舗ＣＤ
        07  TBL-NBASYO      PIC  X(01).  *> 納品場所
        07  TBL-TENYMD      PIC  9(08).  *> 店着日
        07  TBL-HAKOSU      PIC  9(06).  *> 出荷総梱数
        07  TBL-TEIHAK      PIC  9(06).  *> 訂正箱数
    03  TBL-HAKO-G2R  REDEFINES TBL-HAKO-G2.
      05  TBL-HAKO-G3R  OCCURS 60.   *> ページ数
        07  TBL-HAKO-G4R  OCCURS 17. *> 行数
          09  TBL-KAKUNO-FGR  PIC  X(01).
          09  TBL-TENCDR    PIC  9(05).
          09  TBL-NBASYOR   PIC  X(01).
          09  TBL-TENYMDR   PIC  9(08).
          09  TBL-HAKOSUR   PIC  9(06).
          09  TBL-TEIHAKR   PIC  9(06).

 01  TBL-HAKO-IXMAX         PIC  9(05).
 01  TBL-HAKO-PGMAX         PIC  9(03).
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了　　　　　　　　".
     03  PF-MSG2.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了　_項目戻し　　".
     03  PF-MSG3.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了　_項目戻し　_前頁　_次頁".
 01  PF-MSG-AREA-R  REDEFINES PF-MSG-AREA.
     03  PF-MSG-R  OCCURS 3 PIC  N(30).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(25)
             VALUE NC"正しい値を入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(25)
             VALUE NC"対象データが存在しません。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(25)
             VALUE NC"区分を入力して下さい。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(25)
             VALUE NC"バッチＮＯを入力して下さい。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(25)
             VALUE NC"管理番号を入力して下さい。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(25)
             VALUE NC"無効キーです。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(25)
             VALUE NC"前頁はありません。".
     03  ERR-MSG8.
         05  FILLER              PIC   N(25)
             VALUE NC"次頁はありません。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(25)
             VALUE NC"作場コードを入力して下さい。".
     03  ERR-MSG10.
         05  FILLER              PIC   N(25)
             VALUE NC"店着日を入力して下さい。".
     03  ERR-MSG11.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG12.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG13.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG14.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  14  PIC   N(25).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  HK1-ERR           PIC N(20) VALUE
                        NC"箱数ファイル１エラー".
     03  HK2-ERR           PIC N(20) VALUE
                        NC"箱数ファイル２エラー".
     03  SKB-ERR           PIC N(20) VALUE
                        NC"作場マスタエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"店舗マスタエラー".
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
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 HK1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFHAKOF.
     DISPLAY     HK1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HK1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 HK2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFHAKOF2.
     DISPLAY     HK2-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HK2-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SKB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SKB-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SKB-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
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
     OPEN  I-O   NFHAKOF.
     OPEN  INPUT NFHAKOF2.
     OPEN  INPUT SAKUBAF.
     OPEN  INPUT HTENMS.
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
*キー項目入力
         WHEN      "2"  PERFORM   DSP-HEAD21-SEC
*キー項目入力２
         WHEN      "3"  PERFORM   DSP-HEAD22-SEC
*明細項目入力
         WHEN      "4"  PERFORM   DSP-BODY-SEC
*確認入力
         WHEN      "5"  PERFORM   DSP-KAKU-SEC
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
                IF  DSP-BCHNO1 = ZERO
                    MOVE  SPACE            TO  DSP-BCHNO1(1:8)
                END-IF
                IF  DSP-BCHNO2 = ZERO
                    MOVE  SPACE            TO  DSP-BCHNO2(1:4)
                END-IF
                IF  DSP-BCHNO3 = ZERO
                    MOVE  SPACE            TO  DSP-BCHNO3(1:8)
                END-IF
                IF  DSP-KNRINO = ZERO
                    MOVE  SPACE            TO  DSP-KNRINO(1:8)
                END-IF
                IF  DSP-TENYMD = ZERO
                    MOVE  SPACE            TO  DSP-TENYMD(1:8)
                END-IF
                IF SAV-SHORI = 2
                   MOVE  99999999     TO  DSP-BCHNO1
                   MOVE  9999         TO  DSP-BCHNO2
*******************MOVE  99999999     TO  DSP-BCHNO3
                END-IF
                MOVE  DSP-BCHNO1      TO  SV-HD-BCHNO1
                MOVE  DSP-BCHNO2      TO  SV-HD-BCHNO2
                MOVE  DSP-BCHNO3      TO  SV-HD-BCHNO3
                MOVE  DSP-KNRINO      TO  SV-HD-KNRINO
                MOVE  DSP-TENYMD      TO  SV-HD-TENYMD
                MOVE  DSP-SKBCD       TO  SV-HD-SKBCD
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
*キー項目２
         WHEN   "3"
                IF  DSP-KNRINO = ZERO
                    MOVE  SPACE            TO  DSP-KNRINO(1:8)
                END-IF
                IF  DSP-TENYMD = ZERO
                    MOVE  SPACE            TO  DSP-TENYMD(1:8)
                END-IF
                MOVE  DSP-BCHNO1      TO  SV-HD-BCHNO1
                MOVE  DSP-BCHNO2      TO  SV-HD-BCHNO2
                MOVE  DSP-BCHNO3      TO  SV-HD-BCHNO3
                MOVE  DSP-KNRINO      TO  SV-HD-KNRINO
                MOVE  DSP-TENYMD      TO  SV-HD-TENYMD
                MOVE  DSP-SKBCD       TO  SV-HD-SKBCD
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
*明細項目
         WHEN   "4"
                MOVE    PF-MSG-R(3)        TO   DSP-PFGAID
*確認
         WHEN   "5"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY37611"          TO   DSP-FMT.
     WRITE    DSP-FSY37611.
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
*キー項目２
         WHEN   "3"
                MOVE    "KEYGR2"  TO   DSP-GRP
*明細項目
         WHEN   "4"
                MOVE    "MAIN"    TO   DSP-GRP
*確認
         WHEN   "5"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FSY37611"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             処理区分チェック                                 *
****************************************************************
 HEAD1-CHK-SEC             SECTION.
*処理区分 未入力はエラー
     IF     DSP-KBN NOT NUMERIC
         OR DSP-KBN = ZERO
         MOVE  3                 TO  ERR-FLG
         MOVE  SPACE             TO  DSP-KBN (1:1)
         GO TO  HEAD1-CHK-EXIT
     END-IF.

     IF  DSP-KBN = 1 OR 2
         IF  DSP-KBN = 1   *> オンライン
             MOVE  "2"           TO  PSW
         ELSE              *> 手書き
             MOVE 99999999       TO  DSP-BCHNO1
             MOVE 9999           TO  DSP-BCHNO2
*************MOVE 99999999       TO  DSP-BCHNO3
             MOVE  "3"           TO  PSW
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
*             キー項目入力( PSW = 2 )                          *
****************************************************************
 DSP-HEAD21-SEC         SECTION.
     MOVE     "DSP-HEAD21-SEC"    TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM  HEAD2-CHK-SEC
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
                GO       TO      DSP-HEAD21-SEC
     END-EVALUATE.
*
 DSP-HEAD21-EXIT.
     EXIT.
****************************************************************
*    キー項目チェック                                          *
****************************************************************
 HEAD2-CHK-SEC         SECTION.
     INITIALIZE  TBL-HAKO-G.

     PERFORM  HEAD2B-CHK-SEC.
     IF  ERR-FLG = ZERO
         PERFORM  MS-EDT-SEC
         IF  ERR-FLG = ZERO
             MOVE  "4"      TO  PSW
         END-IF

     END-IF.
 HEAD2-CHK-EXIT.
     EXIT.
****************************************************************
*    キー項目チェックＢ                                        *
****************************************************************
 HEAD2B-CHK-SEC        SECTION.
     MOVE  "HEAD2-CHK-SEC"       TO  S-NAME.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO1.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO2.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO3.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KNRINO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KNRINO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TENYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENYMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SKBCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SKBCD.
* バッチＮＯ（日付）
     IF  DSP-KBN = "1"  *> オンライン
         IF     (DSP-BCHNO1  NOT NUMERIC  OR DSP-BCHNO1 = ZERO)
            AND (DSP-BCHNO2  NOT NUMERIC  OR DSP-BCHNO2 = ZERO)
            AND (DSP-BCHNO3  NOT NUMERIC  OR DSP-BCHNO3 = ZERO)
            AND (DSP-KNRINO  NOT NUMERIC  OR DSP-KNRINO = ZERO)
            MOVE  4              TO  ERR-FLG
            MOVE  "C"            TO  EDIT-CURSOR OF DSP-BCHNO1
            MOVE  "R"            TO  EDIT-OPTION OF DSP-BCHNO1
            MOVE  "R"            TO  EDIT-OPTION OF DSP-BCHNO2
            MOVE  "R"            TO  EDIT-OPTION OF DSP-BCHNO3
            MOVE  "R"            TO  EDIT-OPTION OF DSP-KNRINO
            GO TO  HEAD2B-CHK-EXIT
         END-IF

         IF     DSP-BCHNO1 NOT NUMERIC
             OR DSP-BCHNO1 = ZERO
             CONTINUE
         ELSE
             MOVE  "2"           TO  LINK-IN-KBN
             MOVE  DSP-BCHNO1       TO  LINK-IN-YMD8
             CALL  "SKYDTCKB" USING LINK-IN-KBN
                                    LINK-IN-YMD6
                                    LINK-IN-YMD8
                                    LINK-OUT-RET
                                    LINK-OUT-YMD
             IF  LINK-OUT-RET NOT = ZERO
                 IF  ERR-FLG = ZERO
                     MOVE  1     TO  ERR-FLG
                 END-IF
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-BCHNO1
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-BCHNO1
                 GO TO    HEAD2B-CHK-EXIT
             END-IF
         END-IF
* バッチＮＯ（時間）

* バッチＮＯ（取引先）
         IF      (    DSP-BCHNO1  IS NUMERIC
                  AND DSP-BCHNO1  NOT = ZERO)
             AND (    DSP-BCHNO3  NOT NUMERIC
                  OR  DSP-BCHNO3  = ZERO)
             IF  ERR-FLG = ZERO
                 MOVE  1         TO  ERR-FLG
             END-IF
             MOVE  "C"           TO  EDIT-CURSOR OF DSP-BCHNO3
             MOVE  "R"           TO  EDIT-OPTION OF DSP-BCHNO3
             GO TO  HEAD2B-CHK-EXIT
         END-IF

* 管理番号

     ELSE               *> 手書き
* 管理番号
         IF      DSP-KNRINO  NOT NUMERIC
             OR  DSP-KNRINO = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  5         TO  ERR-FLG
             END-IF
             MOVE  SPACE         TO  DSP-KNRINO(1:8)
             MOVE  "C"           TO  EDIT-CURSOR OF DSP-KNRINO
             MOVE  "R"           TO  EDIT-OPTION OF DSP-KNRINO
             GO TO  HEAD2B-CHK-EXIT
         END-IF
     END-IF.

* 店着日
     IF     DSP-TENYMD NOT NUMERIC
         OR DSP-TENYMD = ZERO
         IF  ERR-FLG = ZERO
             MOVE  10       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TENYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TENYMD
         GO TO    HEAD2B-CHK-EXIT
     END-IF.

     MOVE  "2"              TO  LINK-IN-KBN.
     MOVE  DSP-TENYMD       TO  LINK-IN-YMD8.
     CALL  "SKYDTCKB" USING LINK-IN-KBN
                            LINK-IN-YMD6
                            LINK-IN-YMD8
                            LINK-OUT-RET
                            LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG = ZERO
             MOVE  1        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TENYMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TENYMD
         GO TO    HEAD2B-CHK-EXIT
     END-IF.

* 作場ＣＤ
     IF  DSP-SKBCD = SPACE
         IF  ERR-FLG = ZERO
             MOVE  9        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SKBCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SKBCD
         GO TO  HEAD2B-CHK-EXIT
     END-IF.

     MOVE  DSP-SKBCD        TO  SKB-F01.
     PERFORM  RD-SAKUBAF-SEC.
     IF  FG-SAKUBAF-INV = 1
         IF  ERR-FLG = ZERO
             MOVE  1        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SKBCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SKBCD
         GO TO  HEAD2B-CHK-EXIT
     END-IF.

     IF  ERR-FLG = ZERO
         PERFORM  NFHAKO-CHK-SEC
         IF  ERR-FLG NOT = ZERO
             GO TO  HEAD2B-CHK-EXIT
         END-IF
     END-IF.

 HEAD2B-CHK-EXIT.
     EXIT.

****************************************************************
*    作場マスタ検索                                            *
****************************************************************
 RD-SAKUBAF-SEC          SECTION.
     READ  SAKUBAF
       INVALID
         MOVE  1                 TO  FG-SAKUBAF-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-SAKUBAF-INV
     END-READ.

 RD-SAKUBAF-EXIT.
     EXIT.

****************************************************************
*    箱数ファイル存在チェック                                  *
****************************************************************
 NFHAKO-CHK-SEC         SECTION.
     IF  DSP-KBN = "1"  *> オンライン
         IF  DSP-BCHNO1 IS NUMERIC AND DSP-BCHNO1 NOT = ZERO
               *> バッチNO指定
             MOVE  LOW-VALUE     TO FG-NFHAKOF2-END
             MOVE  DSP-BCHNO1    TO  HK2-F02  *> バッチ日付
             MOVE  DSP-BCHNO2    TO  HK2-F03  *> バッチ時間
             MOVE  DSP-BCHNO3    TO  HK2-F04  *> バッチ取引先
             MOVE  DSP-SKBCD     TO  HK2-F05  *> 作場ＣＤ
             MOVE  ZERO          TO  HK2-F06  *> 店舗ＣＤ
             MOVE  SPACE         TO  HK2-F07  *> 納品場所
             MOVE  ZERO          TO  HK2-F08  *> 店着日
             PERFORM  RD-NFHAKOF2-SEC
             IF  FG-NFHAKOF2-END = "END"
                 IF  ERR-FLG = ZERO
                     MOVE  2     TO  ERR-FLG
                 END-IF
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-BCHNO1
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-BCHNO1
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-BCHNO2
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-BCHNO3
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-KNRINO
                 GO TO  NFHAKO-CHK-EXIT
             END-IF

             MOVE  HK2-F01       TO  TBL-KANRNO
             IF     DSP-KNRINO NOT NUMERIC
                 OR DSP-KNRINO = ZERO
                 MOVE  HK2-F01   TO  DSP-KNRINO
             END-IF

         ELSE  *> 管理番号指定
             MOVE  LOW-VALUE     TO  FG-NFHAKOF-END
             MOVE  DSP-KNRINO    TO  WK-RD-KANRNO
             MOVE  DSP-KNRINO    TO  HK1-F01  *> 管理番号
             MOVE  DSP-SKBCD     TO  HK1-F05  *> 作場ＣＤ
             MOVE  ZERO          TO  HK1-F06  *> 店舗ＣＤ
             MOVE  SPACE         TO  HK1-F07  *> 納品場所
             MOVE  ZERO          TO  HK1-F08  *> 店着日
             PERFORM  RD-NFHAKOF-SEC
             IF  FG-NFHAKOF-END = "END"
                 IF  ERR-FLG = ZERO
                     MOVE  2     TO  ERR-FLG
                 END-IF
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-KNRINO
                 GO TO  NFHAKO-CHK-EXIT
             ELSE
                 IF HK1-F02 NOT = 99999999 *> バッチ日付
                    CONTINUE
                 ELSE
                     MOVE  2     TO  ERR-FLG
                     MOVE  "R"   TO  EDIT-OPTION OF DSP-KNRINO
                     GO TO  NFHAKO-CHK-EXIT
                 END-IF
             END-IF

             MOVE  HK1-F01        TO  TBL-KANRNO
             MOVE  HK1-F02        TO  DSP-BCHNO1
             MOVE  HK1-F03        TO  DSP-BCHNO2
             MOVE  HK1-F04        TO  DSP-BCHNO3

         END-IF

     ELSE               *> 手書き
         MOVE  LOW-VALUE    TO  FG-NFHAKOF-END
         MOVE  DSP-KNRINO   TO  WK-RD-KANRNO
         MOVE  DSP-KNRINO   TO  HK1-F01  *> 管理番号
         MOVE  DSP-SKBCD    TO  HK1-F05  *> 作場ＣＤ
         MOVE  ZERO         TO  HK1-F06  *> 店舗ＣＤ
         MOVE  SPACE        TO  HK1-F07  *> 納品場所
         MOVE  ZERO         TO  HK1-F08  *> 店着日
         PERFORM  RD-NFHAKOF-SEC
         IF  FG-NFHAKOF-END = "END"
             IF  ERR-FLG = ZERO
                 MOVE  2    TO  ERR-FLG
             END-IF
             MOVE  "R"      TO  EDIT-OPTION OF DSP-KNRINO
             GO TO  NFHAKO-CHK-EXIT
         ELSE
             IF HK1-F02 = 99999999 *> バッチ日付
                CONTINUE
             ELSE
                 MOVE  2     TO  ERR-FLG
                 MOVE  "R"   TO  EDIT-OPTION OF DSP-KNRINO
                 GO TO  NFHAKO-CHK-EXIT
             END-IF
         END-IF

         MOVE  HK1-F01       TO  TBL-KANRNO
         MOVE  HK1-F01       TO  DSP-KNRINO

     END-IF.

 NFHAKO-CHK-EXIT.
     EXIT.
****************************************************************
*    箱数ファイル２読込み　                                    *
****************************************************************
 RD-NFHAKOF2-SEC         SECTION.

     IF  FG-NFHAKOF2-END = LOW-VALUE
         START  NFHAKOF2  KEY >= HK2-F02
                                 HK2-F03
                                 HK2-F04
                                 HK2-F05
                                 HK2-F06
                                 HK2-F07
                                 HK2-F08
           INVALID KEY
              MOVE  "END"   TO  FG-NFHAKOF2-END
              GO TO  RD-NFHAKOF2-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFHAKOF2-END

     END-IF.

 RD-NFHAKOF2-010.
     READ  NFHAKOF2  NEXT
       AT  END
         MOVE  "END"        TO  FG-NFHAKOF2-END
         GO TO  RD-NFHAKOF2-EXIT
     END-READ.

     IF  HK2-F02 > DSP-BCHNO1
         MOVE  "END"        TO  FG-NFHAKOF2-END
         GO TO  RD-NFHAKOF2-EXIT
     END-IF.

     IF  HK2-F03 > DSP-BCHNO2
         MOVE  "END"        TO  FG-NFHAKOF2-END
         GO TO  RD-NFHAKOF2-EXIT
     END-IF.

     IF  HK2-F04 > DSP-BCHNO3
         MOVE  "END"        TO  FG-NFHAKOF2-END
         GO TO  RD-NFHAKOF2-EXIT
     END-IF.

     IF     DSP-KNRINO  NOT NUMERIC
         OR DSP-KNRINO = ZERO
         CONTINUE
     ELSE
         IF  HK2-F01 NOT = DSP-KNRINO
             GO TO  RD-NFHAKOF2-010
         END-IF
     END-IF.

     IF      DSP-SKBCD NOT NUMERIC
         OR  DSP-SKBCD = ZERO
         CONTINUE
     ELSE
         IF  HK2-F05 > DSP-SKBCD
             MOVE  "END"        TO  FG-NFHAKOF2-END
             GO TO  RD-NFHAKOF2-EXIT
         END-IF
     END-IF.

     IF     DSP-TENYMD NOT NUMERIC
         OR DSP-TENYMD = ZERO
         CONTINUE
     ELSE
         IF  HK2-F08 NOT = DSP-TENYMD
             GO TO  RD-NFHAKOF2-010
         END-IF
     END-IF.

 RD-NFHAKOF2-EXIT.
     EXIT.

****************************************************************
*    箱数ファイル読込み　                                      *
****************************************************************
 RD-NFHAKOF-SEC          SECTION.

     IF  FG-NFHAKOF-END = LOW-VALUE
         START  NFHAKOF  KEY >= HK1-F01
                                HK1-F05
                                HK1-F06
                                HK1-F07
                                HK1-F08
           INVALID KEY
              MOVE  "END"   TO  FG-NFHAKOF-END
              GO TO  RD-NFHAKOF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFHAKOF-END

     END-IF.

 RD-NFHAKOF-010.
     READ  NFHAKOF  NEXT
       AT  END
         MOVE  "END"        TO  FG-NFHAKOF-END
         GO TO  RD-NFHAKOF-EXIT
     END-READ.

     IF  HK1-F01 > WK-RD-KANRNO
         MOVE  "END"        TO  FG-NFHAKOF-END
         GO TO  RD-NFHAKOF-EXIT
     END-IF.

     IF      DSP-SKBCD NOT NUMERIC
         OR  DSP-SKBCD = ZERO
         CONTINUE
     ELSE
         IF  HK1-F05 > DSP-SKBCD
             MOVE  "END"        TO  FG-NFHAKOF-END
             GO TO  RD-NFHAKOF-EXIT
         END-IF
     END-IF.

     IF     DSP-TENYMD NOT NUMERIC
         OR DSP-TENYMD = ZERO
         CONTINUE
     ELSE
         IF  HK1-F08 NOT = DSP-TENYMD
             GO TO  RD-NFHAKOF-010
         END-IF
     END-IF.


 RD-NFHAKOF-EXIT.
     EXIT.

****************************************************************
*  明細編集処理                                                *
****************************************************************
 MS-EDT-SEC         SECTION.

* 対象データ内部テーブル格納
     MOVE  ZERO             TO  IX.
     MOVE  LOW-VALUE        TO  FG-NFHAKOF-END.
     MOVE  TBL-KANRNO       TO  WK-RD-KANRNO.
     MOVE  TBL-KANRNO       TO  HK1-F01.  *> 管理番号
     MOVE  DSP-SKBCD        TO  HK1-F05.  *> 作場ＣＤ
     MOVE  ZERO             TO  HK1-F06.  *> 店舗ＣＤ
     MOVE  SPACE            TO  HK1-F07.  *> 納品場所
     MOVE  ZERO             TO  HK1-F08.  *> 店着日
     PERFORM  RD-NFHAKOF-SEC.

     PERFORM  UNTIL  FG-NFHAKOF-END = "END"
       ADD 1  TO IX
       IF  IX <= 1020
           MOVE  IX         TO  TBL-HAKO-IXMAX
           MOVE  "1"        TO  TBL-KAKUNO-FG (IX)
           MOVE  HK1-F06    TO  TBL-TENCD  (IX) *> 店舗ＣＤ
           MOVE  HK1-F07    TO  TBL-NBASYO (IX) *> 納品場所
           MOVE  HK1-F08    TO  TBL-TENYMD (IX) *> 店着日
           MOVE  HK1-F09    TO  TBL-HAKOSU (IX) *> 出荷総梱数
           MOVE  HK1-F09    TO  TBL-TEIHAK (IX) *> 訂正箱数
       ELSE
           DISPLAY "SSY3761I TBL-HAKO TBL OVER ]]]]"  UPON CONS
       END-IF

       PERFORM  RD-NFHAKOF-SEC
     END-PERFORM.

* 最大格納ページ計算
     MOVE  ZERO            TO  WK-AMARI.
     DIVIDE 17  INTO       TBL-HAKO-IXMAX
                GIVING     TBL-HAKO-PGMAX
                REMAINDER  WK-AMARI.
     IF  WK-AMARI NOT = ZERO
         ADD  1  TO TBL-HAKO-PGMAX
     END-IF.

* 先頭ページ編集
     MOVE  1                TO  IX-PG.
     PERFORM  MS-GMN-EDT-SEC.

 MS-EDT-EXIT.
     EXIT.
****************************************************************
*  明細画面編集処理                                            *
****************************************************************
 MS-GMN-EDT-SEC         SECTION.
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 17
       IF  TBL-KAKUNO-FGR (IX-PG IX) NOT = SPACE
           MOVE  TBL-TENCDR  (IX-PG IX)
                                 TO  DSP-TENCD  (IX)
           MOVE  DSP-BCHNO3      TO  TEN-F52
           MOVE  TBL-TENCDR  (IX-PG IX)
                                 TO  TEN-F011 *> 店舗コード
           READ  HTENMS
             INVALID
               MOVE  SPACE       TO  DSP-TENNM (IX)
             NOT INVALID
               MOVE  TEN-F02     TO  DSP-TENNM (IX)
           END-READ

           MOVE  TBL-NBASYOR (IX-PG IX)  TO  DSP-NBASYO (IX)
           MOVE  TBL-HAKOSUR (IX-PG IX)  TO  DSP-HAKOSU (IX)
           MOVE  TBL-TEIHAKR (IX-PG IX)  TO  DSP-TEIHAK (IX)
           MOVE  SPACE      TO  EDIT-STATUS OF DSP-TEIHAK (IX)
       ELSE
           MOVE  SPACE      TO  DSP-TENCD  (IX)(1:5)
           MOVE  SPACE      TO  DSP-TENNM  (IX)
           MOVE  SPACE      TO  DSP-NBASYO (IX)
           MOVE  SPACE      TO  DSP-HAKOSU (IX)(1:6)
           MOVE  SPACE      TO  DSP-TEIHAK (IX)(1:6)
*        プロテクト属性設定
           MOVE  "X"        TO  EDIT-STATUS OF DSP-TEIHAK (IX)
       END-IF

     END-PERFORM.

 MS-GMN-EDT-EXIT.
     EXIT.
****************************************************************
*             キー項目入力２( PSW = 3 )                        *
****************************************************************
 DSP-HEAD22-SEC         SECTION.
     MOVE     "DSP-HEAD21-SEC"    TO   S-NAME.
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
                GO       TO      DSP-HEAD22-SEC
     END-EVALUATE.
*
 DSP-HEAD22-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 4 )                        *
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
                PERFORM  BODY-CHK-SEC
                IF  ERR-FLG = ZERO
                    MOVE  "5"    TO  PSW
                END-IF
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*前頁
         WHEN   "F011"
           IF  IX-PG = 1
               MOVE  7           TO  ERR-FLG
           ELSE  *> 前頁編集
               PERFORM  GMN-TO-TBL-SEC
               COMPUTE  IX-PG = IX-PG - 1
               PERFORM  MS-GMN-EDT-SEC
           END-IF
*次頁
         WHEN   "F012"
           IF  IX-PG >= TBL-HAKO-PGMAX
               MOVE  8           TO  ERR-FLG
           ELSE  *> 次頁編集
               PERFORM  GMN-TO-TBL-SEC
               COMPUTE  IX-PG = IX-PG + 1
               PERFORM  MS-GMN-EDT-SEC
           END-IF
*項目戻し
         WHEN   "F006"
                IF DSP-KBN = 1
                   MOVE  "2"     TO  PSW
                ELSE
                   MOVE  "3"     TO  PSW
                END-IF
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
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE  "BODY-CHK-SEC"        TO  S-NAME.

     PERFORM  GMN-TO-TBL-SEC.

 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*  現画面内容をＴＢＬに格納する                                *
****************************************************************
 GMN-TO-TBL-SEC             SECTION.

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 17
       IF  TBL-KAKUNO-FGR (IX-PG IX) NOT = SPACE
           MOVE  DSP-TEIHAK (IX)  TO  TBL-TEIHAKR (IX-PG IX)
       END-IF

     END-PERFORM.

 GMN-TO-TBL-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 5 ）
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
                PERFORM  FILE-UPD-SEC
                PERFORM  INIT-DSP-SEC
                IF SAV-SHORI = "1"  *> オンライン
                   MOVE  "2"     TO  PSW
                ELSE              *> 手書き
                   MOVE  "3"     TO  PSW
                END-IF

                MOVE   SAV-SHORI TO   DSP-KBN
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
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
*    箱数ファイル更新                                          *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE  "FILE-WRT-SEC"   TO  S-NAME.

     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > TBL-HAKO-IXMAX
       MOVE  TBL-KANRNO       TO  HK1-F01
       MOVE  DSP-SKBCD        TO  HK1-F05
       MOVE  TBL-TENCD  (IX)  TO  HK1-F06
       MOVE  TBL-NBASYO (IX)  TO  HK1-F07
       MOVE  TBL-TENYMD (IX)  TO  HK1-F08

       READ  NFHAKOF  KEY IS HK1-F01
                             HK1-F05
                             HK1-F06
                             HK1-F07
                             HK1-F08
         INVALID
           MOVE  1          TO  FG-NFHAKOF-INV
         NOT INVALID
           MOVE  ZERO       TO  FG-NFHAKOF-INV
       END-READ

       IF  FG-NFHAKOF-INV = ZERO
           IF TBL-TEIHAK (IX)  NOT = HK1-F09
              MOVE  TBL-TEIHAK (IX)  TO  HK1-F09 *> 出荷総梱数
              REWRITE  HK1-REC
           END-IF
       ELSE
           DISPLAY "SSY3761I NFHAKOF RECORD NOT FOUND ??? KEY="
                       TBL-KANRNO
                   "," DSP-BCHNO1
                   "," DSP-BCHNO2
                   "," DSP-BCHNO3
                   "," DSP-SKBCD
                   "," TBL-TENCD  (IX)
                   "," TBL-NBASYO (IX)
                   "," TBL-TENYMD (IX)  UPON CONS
       END-IF

     END-PERFORM.

 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.
*画面の初期化
     MOVE  SPACE            TO  DSP-FSY37611.
*ＰＧＩＤ
     MOVE  "SSY3761I"       TO  DSP-PGID.
*システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*リバース，カーソルパーク解除
***  メッセージＮＯ
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO1.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO2.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BCHNO3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BCHNO3.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KNRINO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KNRINO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TENYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENYMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SKBCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SKBCD.
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 17
       MOVE  "M"            TO  EDIT-OPTION OF DSP-HAKOSU (IX)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-HAKOSU (IX)

     END-PERFORM.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  NFHAKOF.
     CLOSE  NFHAKOF2.
     CLOSE  SAKUBAF.
     CLOSE  HTENMS.
     CLOSE  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3761I   END PROGRAM  >>******************

```
