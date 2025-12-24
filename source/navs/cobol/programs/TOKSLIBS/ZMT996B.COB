****************************************************************
*
*    顧客名　　　　　　　：　（株）サカタのタネ殿
*    サブシステム　　　　：　マスタ管理　　　　　
*    業務名　　　　　　　：　マスタメンテ
*    モジュール名　　　　：　_卸一括更新
*                        ：　ＺＭＴ９９６Ｂ
*    処理概要　　　　　　：
*      _番一括変更入力で登録したメンテデータを商品変換テーブル
*      に更新します。
*    作成日／更新日　　　：　2011/11/24
*    作成者／更新者　　　：　飯田/NAV
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           ZMT996B.
 AUTHOR.               TAKEI.
 DATE-WRITTEN.         05/11/08.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
* 画面定義ファイル
     SELECT  DSPFILE  ASSIGN    TO        GS-DSPF
                      FORMAT              DSP-FMT
                      GROUP               DSP-GRP
                      PROCESSING          DSP-PRO
                      FUNCTION            DSP-FNC
                      FILE      STATUS    DSP-ST.
*----<< _番一括メンテデータ >>-*
     SELECT  TANAMTBL ASSIGN    TO        TANAMTB3
                      ORGANIZATION        INDEXED
                      ACCESS    MODE      DYNAMIC
                      RECORD    KEY
                        TNM-F02 *> 量販店商品ＣＤ
                        TNM-F04 *> 出荷場所
                        TNM-F20 *> 旧_番
                        TNM-F01 *> 取引先ＣＤ
                        TNM-F03 *> 自社商品ＣＤ
                      FILE      STATUS    TNM-ST.
*----<< 商品変換テーブル >>-*
     SELECT  HSHOTBL    ASSIGN    TO      SHOTBL1
                       ORGANIZATION       INDEXED
                       ACCESS    MODE     DYNAMIC
                       RECORD    KEY
                         STB-F01 *> 相手取引先ＣＤ
                         STB-F02 *> 相手取商品ＣＤ
                       FILE      STATUS    STB-ST.
*----<< 商品変換テーブル履歴ファイル >>-*
     SELECT  HSHOTBR   ASSIGN    TO       HSHOTBR1
                       ORGANIZATION       INDEXED
                       ACCESS    MODE     DYNAMIC
                       RECORD    KEY
                         STR-F01 *> 入力日
                         STR-F02 *> 入力時刻
                         STR-F03 *> 担当者
                       STATUS              STR-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル
****************************************************************
 FD  DSPFILE.
     COPY      FMT996    OF   XMDLIB
     JOINING   DSP       AS   PREFIX.
****************************************************************
*    FILE = 一括メンテデータ                                   *
****************************************************************
 FD  TANAMTBL
     LABEL     RECORD    IS   STANDARD.
     COPY      TANAMTBL  OF   XFDLIB
     JOINING   TNM       AS   PREFIX.
****************************************************************
*    FILE = 商品変換テーブル                                   *
****************************************************************
 FD  HSHOTBL
     LABEL     RECORD    IS   STANDARD.
     COPY      HSHOTBL   OF   XFDLIB
     JOINING   STB       AS   PREFIX.
****************************************************************
*    FILE = 商品変換テーブル履歴ファイル
****************************************************************
 FD  HSHOTBR
     LABEL     RECORD    IS   STANDARD.
     COPY      HSHOTBR   OF   XFDLIB
     JOINING   STR       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  TNM-ST                   PIC  X(02).
     03  STB-ST                   PIC  X(02).
     03  STR-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  FG-HSHOTBL-INV           PIC  9(01).
*カウンター
 01  CNT-AREA.
     03  CT-IN                   PIC  9(06)  VALUE  ZERO.
     03  CT-UPD                  PIC  9(06)  VALUE  ZERO.
     03  CT-SKP                  PIC  9(06)  VALUE  ZERO.
*システム日付格納
 01  SYS-DATE                     PIC  9(06)  VALUE  ZERO.
*システム日付格納（８桁）
 01  WK-DATE.
     03  WK-YY                    PIC  9(04)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
     03  WK-DD                    PIC  9(02)  VALUE  ZERO.
*システム時間格納
 01  WK-TIME.
     03  WK-TIME-1                PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-2                PIC  9(04)  VALUE  ZERO.

 01  S-NAME-SV              PIC  X(20).

*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).

*ファイルエラーメッセージ
 01  FILE-ERR.
     03  DSP-ERR           PIC N(15) VALUE
         NC"画面ファイルエラー".
     03  TNM-ERR           PIC N(15) VALUE
         NC"_卸一括メンテデータエラー".
     03  STB-ERR           PIC N(15) VALUE
         NC"商品変換テーブルエラー".
     03  STR-ERR           PIC N(15) VALUE
         NC"商品変換テーブル履歴Ｆエラー".
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
**************************************************************
 LINKAGE               SECTION.
* 入力パラメータ
 01  PA-IN-TANCD            PIC X(02).
 01  PA-IN-BMNCD            PIC X(04).
* 出力パラメータ
 01  PA-OT-NYURYKBI         PIC 9(08).
 01  PA-OT-NYURYKTM         PIC 9(04).
*
**************************************************************
 PROCEDURE         DIVISION   USING  PA-IN-TANCD
                                     PA-IN-BMNCD
                                     PA-OT-NYURYKBI
                                     PA-OT-NYURYKTM.
**************************************************************
 DECLARATIVES.
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
 TNM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TANAMTBL.
     MOVE        TNM-ST    TO        E-ST.
     MOVE        "TANAMTBL"  TO      E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TNM-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 STB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     MOVE        STB-ST    TO        E-ST.
     MOVE        "HSHOTBL" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     STB-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 STR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBR.
     MOVE        STR-ST    TO        E-ST.
     MOVE       "HSHOTBR1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     STR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.
     STOP RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-SEC"       TO  S-NAME.
*ファイルのＯＰＥＮ
     OPEN  I-O     DSPFILE.
     OPEN  INPUT   TANAMTBL.
     OPEN  I-O     HSHOTBL.
     OPEN  I-O     HSHOTBR.
*システム日付取得／時刻取得
     ACCEPT  SYS-DATE  FROM DATE.
     ACCEPT  WK-TIME   FROM TIME.
     MOVE  WK-TIME(1:4)     TO  PA-OT-NYURYKTM.
*システム日付６桁→８桁変換（サブ日付チェック／変換）
 INIT010.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  SYS-DATE         TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"   USING LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  WK-DATE.
     MOVE  LINK-OUT-YMD     TO  PA-OT-NYURYKBI.
*ＰＧスタートメッセージ
     DISPLAY "***ZMT996B START " WK-YY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.

* 画面の表示
     MOVE  SPACE            TO  DSP-FMT996.
     MOVE  "ZMT996B"        TO  DSP-PGID.
     MOVE  WK-DATE          TO  DSP-SDATE.
     MOVE  WK-TIME (1:6)    TO  DSP-STIME.

     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FMT996"         TO  DSP-FMT.
     WRITE  DSP-FMT996.

* 画面読込み
 INIT-010.
     MOVE  "NE"             TO  DSP-PRO.
     MOVE  "KAKU"           TO  DSP-GRP.
     MOVE  "FMT996"         TO  DSP-FMT.
     READ  DSPFILE.
     MOVE  SPACE            TO  DSP-PRO.
* キー判定
     EVALUATE  DSP-FNC
       WHEN  "E000"  *> 実行
         CONTINUE

       WHEN  "F009"  *> 打ち切り
         CLOSE  DSPFILE
         MOVE  4010         TO  PROGRAM-STATUS
         EXIT PROGRAM

       WHEN   OTHER
         GO TO  INIT-010

     END-EVALUATE.

     CLOSE  DSPFILE.

     PERFORM TANAMTBL-READ-SEC.

     MOVE  S-NAME-SV        TO  S-NAME.
 INIT-EXIT.
     EXIT.
****************************************************************
*    一括メンテデータ読込み
****************************************************************
 TANAMTBL-READ-SEC      SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "TANAMTBL-READ-SEC"   TO   S-NAME.

     READ  TANAMTBL NEXT
       AT END
         MOVE  "END"        TO  END-FLG
         GO TO  TANAMTBL-READ-090
     END-READ.

     ADD  1   TO  CT-IN.
     IF  TNM-F10 = "9"
         ADD  1   TO  CT-SKP
         GO TO  TANAMTBL-READ-SEC
     END-IF.

 TANAMTBL-READ-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 TANAMTBL-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     PERFORM  UPD-SEC.

*    次レコード読込み
     PERFORM TANAMTBL-READ-SEC.

     MOVE  S-NAME-SV        TO  S-NAME.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  更新処理
****************************************************************
 UPD-SEC      SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "UPD-SEC"        TO  S-NAME.

     MOVE  TNM-F01          TO  STB-F01.
     MOVE  TNM-F02          TO  STB-F02.

     PERFORM   HSHOTBL-READ-SEC.
     IF  FG-HSHOTBL-INV  = 1
         ADD  1   TO  CT-SKP
         GO TO  UPD-090
     END-IF.

* 商品変換テーブル履歴ファイル更新前編集
     MOVE  SPACE            TO  STR-REC.
     INITIALIZE  STR-REC.
     MOVE  STB-F01          TO  STR-FA01.  *> 前相手取引先ＣＤ
     MOVE  STB-F02          TO  STR-FA02.  *> 前相手商品ＣＤ
     MOVE  STB-F031         TO  STR-FA031. *> 前商品ＣＤ
     MOVE  STB-F032         TO  STR-FA032. *> 前品単
     MOVE  STB-F04          TO  STR-FA04.  *> 前出荷場所
     MOVE  STB-F05          TO  STR-FA05.  *> 前原価単価
     MOVE  STB-F06          TO  STR-FA06.  *> 前売価単価
     MOVE  STB-F07          TO  STR-FA07.  *> 前分類ＣＤ
     MOVE  STB-F08          TO  STR-FA08.  *> 前_番
     MOVE  STB-F09          TO  STR-FA09.  *> 前仕入単価
     MOVE  STB-F10          TO  STR-FA10.  *> 前予備１
     MOVE  STB-F11          TO  STR-FA11.  *> 前検品Ｇ－ＣＤ
     MOVE  STB-F12          TO  STR-FA12.  *> 前出荷地域区分
     MOVE  STB-F13          TO  STR-FA13.  *> 前登録担当者
     MOVE  STB-F14          TO  STR-FA14.  *> 前最終更新担当者
     MOVE  STB-F98          TO  STR-FA98.  *> 前登録日
     MOVE  STB-F99          TO  STR-FA99.  *> 前更新日

* 商品変換テーブル編集
     MOVE  TNM-F05          TO  STB-F05. *> 原価単価
     MOVE  TNM-F06          TO  STB-F06. *> 売価単価
     MOVE  TNM-F08          TO  STB-F08. *> _番
     MOVE  TNM-F09          TO  STB-F09. *> 仕入単価
     MOVE  PA-IN-TANCD      TO  STB-F14. *> 最終更新担当者
     MOVE  TNM-F99          TO  STB-F99. *> レコード更新日

* 商品変換テーブル履歴ファイル更新後編集
     MOVE  PA-OT-NYURYKBI   TO  STR-F01.   *> 入力日
     MOVE  PA-OT-NYURYKTM   TO  STR-F02.   *> 入力時刻
     MOVE  PA-IN-TANCD      TO  STR-F03.   *> 入力担当者
     MOVE  "2"              TO  STR-F04.   *> モード
     MOVE  STB-F01          TO  STR-FB01.  *> 後相手取引先ＣＤ
     MOVE  STB-F02          TO  STR-FB02.  *> 後相手商品ＣＤ
     MOVE  STB-F031         TO  STR-FB031. *> 後商品ＣＤ
     MOVE  STB-F032         TO  STR-FB032. *> 後品単
     MOVE  STB-F04          TO  STR-FB04.  *> 後出荷場所
     MOVE  STB-F05          TO  STR-FB05.  *> 後原価単価
     MOVE  STB-F06          TO  STR-FB06.  *> 後売価単価
     MOVE  STB-F07          TO  STR-FB07.  *> 後分類ＣＤ
     MOVE  STB-F08          TO  STR-FB08.  *> 後_番
     MOVE  STB-F09          TO  STR-FB09.  *> 後仕入単価
     MOVE  STB-F10          TO  STR-FB10.  *> 後予備１
     MOVE  STB-F11          TO  STR-FB11.  *> 後検品Ｇ－ＣＤ
     MOVE  STB-F12          TO  STR-FB12.  *> 後出荷地域区分
     MOVE  STB-F13          TO  STR-FB13.  *> 後登録担当者
     MOVE  STB-F14          TO  STR-FB14.  *> 後最終更新担当者
     MOVE  STB-F98          TO  STR-FB98.  *> 後登録日
     MOVE  STB-F99          TO  STR-FB99.  *> 後更新日
* 商品変換テーブル履歴ファイル出力
     WRITE  STR-REC.

* 商品変換テーブル出力
     REWRITE   STB-REC.
     ADD  1   TO  CT-UPD.

 UPD-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 UPD-EXIT.
     EXIT.

****************************************************************
*  商品変換テーブル読込み（乱）
****************************************************************
 HSHOTBL-READ-SEC      SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "HSHOTBL-READ-SEC"  TO  S-NAME.

     READ  HSHOTBL  KEY IS
                      STB-F01 *> 相手取引先ＣＤ
                      STB-F02 *> 相手取商品ＣＤ
       INVALID
         MOVE  1            TO  FG-HSHOTBL-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HSHOTBL-INV
     END-READ.

     MOVE  S-NAME-SV        TO  S-NAME.
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0
****************************************************************
 END-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "END-SEC"        TO  S-NAME.
* ファイルのクローズ
     CLOSE  TANAMTBL.
     CLOSE  HSHOTBL.
     CLOSE  HSHOTBR.
* 件数メッセージ出力
     DISPLAY  "***ﾒﾝﾃﾃﾞｰﾀ ﾖﾐｺﾐ   ｹﾝｽｳ = "  CT-IN  UPON CONS.
     DISPLAY  "***ﾒﾝﾃﾃﾞｰﾀ SKIP   ｹﾝｽｳ = "  CT-SKP UPON CONS.
     DISPLAY  "***ﾍﾝｶﾝﾃｰﾌﾞﾙ ｺｳｼﾝ ｹﾝｽｳ = "  CT-UPD UPON CONS.
* ＰＧエンドメッセージ
     ACCEPT  WK-TIME   FROM TIME.
     DISPLAY "***ZMT996B END   " WK-YY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.

     MOVE  S-NAME-SV        TO  S-NAME.
 END-EXIT.
     EXIT.
*****************<<  END PROGRAM  >>******************
