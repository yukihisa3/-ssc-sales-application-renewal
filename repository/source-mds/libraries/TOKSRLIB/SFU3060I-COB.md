# SFU3060I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFU3060I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＡＣＯＳ振替改善　　　　　　　　　*
*    業務名　　　　　　　：　ＡＣＯＳ振替改善                  *
*    モジュール名　　　　：　社内振替情報照会　　　　　        *
*    作成日／作成者　　　：　2017.01.26   /  T.TAKAHASHI       *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　社内振替商品の発注／入荷状況を確認*
*                            する。完納にすることも可能とする。*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SFU3060I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         17/01/26.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*----<< 社内振替情報ファイル（Ｌ４） >>-*
     SELECT   SFRHEDF   ASSIGN    TO        DA-01-VI-SFRHEDL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       SFR-F04   SFR-F01
                                            SFR-F02   SFR-F03
                                            SFR-F05   SFR-F06
                                            SFR-F07   SFR-F08
                        FILE      STATUS    SFR-ST.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    SOK-ST.
*----<< 条件ファイル >>-*
     SELECT  HJYOKEN   ASSIGN    TO         DA-01-VI-JYOKEN1
                       ORGANIZATION         INDEXED
                       ACCESS    MODE       RANDOM
                       RECORD    KEY        JYO-F01  JYO-F02
                       FILE      STATUS     JYO-ST.
*----<< 仕入先マスタ >>-*
     SELECT   ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHI-F01
                        FILE      STATUS    SHI-ST.
*----<< 画面定義ファイル  >>-*
     SELECT  DSPFILE   ASSIGN    TO         GS-DSPF
                       FORMAT               DSP-FMT
                       GROUP                DSP-GRP
                       PROCESSING           DSP-PRO
                       FUNCTION             DSP-FNC
                       FILE      STATUS     DSP-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
*----<< 社内振替情報ファイル >>-*
 FD  SFRHEDF            LABEL  RECORD  IS  STANDARD.
     COPY     SFRHEDF   OF     XFDLIB
              JOINING   SFR AS PREFIX.
*----<< 倉庫マスタ >>-*                                      *
 FD  ZSOKMS             LABEL  RECORD  IS  STANDARD.
     COPY     ZSOKMS    OF     XFDLIB
              JOINING   SOK AS PREFIX.
*----<< 条件ファイル >>-*                                    *
 FD  HJYOKEN            LABEL  RECORD  IS  STANDARD.
     COPY     HJYOKEN   OF     XFDLIB
              JOINING   JYO AS PREFIX.
*----<< 仕入先マスタ >>-*                                    *
 FD  ZSHIMS             LABEL  RECORD  IS  STANDARD.
     COPY     ZSHIMS1   OF     XFDLIB
              JOINING   SHI AS PREFIX.
*----<< 画面定義ファイル  >>-*                                 *
 FD  DSPFILE            LABEL  RECORD  IS  OMITTED.
     COPY     FFU30601  OF     XMDLIB
              JOINING   DSP AS PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SFR-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
     03  SHI-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
 01  SET-PGID                     PIC  X(08)  VALUE "SFU3060I".
 01  SET-FORMID                   PIC  X(08)  VALUE "FFU30601".
 01  MAX-PGCNT                    PIC  9(03)  VALUE 50.
 01  MAX-LNCNT                    PIC  9(03)  VALUE 04.
 01  MEISAI-CNT                   PIC  9(09)  VALUE ZERO.
 01  MEIALLCNT                    PIC  9(02)  VALUE ZERO.
*フラグ領域
 01  FLG-AREA.
     03  ZSHIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HJYOKEN-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  SFRHEDF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SFRHEDF-END-FLG          PIC  X(03)  VALUE  SPACE.
     03  P-CNT                    PIC  9(02)  VALUE  ZERO.
     03  S-CNT                    PIC  9(02)  VALUE  ZERO.
     03  C-CNT                    PIC  9(02)  VALUE  ZERO.
     03  Y-CNT                    PIC  9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  IX                       PIC  9(02)  VALUE  ZERO.
     03  IY                       PIC  9(02)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*
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
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
 01  WK-SYOCD.
     03  WK-SYOCD1                PIC  X(08)  VALUE  SPACE.
     03  WK-SYOCD-KU1             PIC  X(01)  VALUE  SPACE.
     03  WK-SYOCD2                PIC  X(05)  VALUE  SPACE.
     03  WK-SYOCD-KU2             PIC  X(01)  VALUE  SPACE.
     03  WK-SYOCD3                PIC  X(02)  VALUE  SPACE.
     03  WK-SYOCD-KU3             PIC  X(01)  VALUE  SPACE.
     03  WK-SYOCD4                PIC  X(01)  VALUE  SPACE.
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-ERRMSG.
         05  FILLER               PIC   N(30)
      VALUE NC"_取消_終了　　　　　　　　　　　".
     03  PF-PFMSG.
         05  FILLER               PIC   N(30)
      VALUE NC"_取消_終了_項目戻り_明細照会_前頁_次頁".
     03  PF-MSG3.
         05  FILLER               PIC   N(30)
      VALUE NC"_取消_終了_項目戻り_前頁_次頁".
     03  PF-MSG4.
         05  FILLER               PIC   N(30)
      VALUE NC"_取消_終了_項目戻り".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   4   PIC   N(30).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG01.
         05  FILLER              PIC   N(20)
             VALUE NC"無効ＰＦキーです。".
     03  ERR-MSG02.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫Ｍ未登録です。！！".
     03  ERR-MSG03.
         05  FILLER              PIC   N(20)
             VALUE NC"仕入先Ｍ未登録です！！".
     03  ERR-MSG04.
         05  FILLER              PIC   N(20)
             VALUE NC"伝区（条件Ｆ）未登録です！！".
     03  ERR-MSG05.
         05  FILLER              PIC   N(20)
             VALUE NC"完納区分は空白、１以外指定出来ません！！".
     03  ERR-MSG06.
         05  FILLER              PIC   N(20)
             VALUE NC"対象データが存在しません！！".
     03  ERR-MSG07.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁がありません！！".
     03  ERR-MSG08.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁がありません！！".
     03  ERR-MSG09.
         05  FILLER              PIC   N(20)
             VALUE NC"明細番号入力エラー！！".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"明細照会番号エラー！！".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG14.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG15.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG16.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG17.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG18.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG19.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG20.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS 20   PIC   N(20).
*メンテデータ退避エリア
 01  TABLE-AREA.
     03  TABLE1      OCCURS  50.
         05  PAGE-SONZAI-CHK      PIC   X(01).
         05  TABLE2  OCCURS  04.
             07  TBL-MEINEN       PIC   9(04).
             07  TBL-MEISEA       PIC   X(02).
             07  TBL-MEIDEK       PIC   X(02).
             07  TBL-MEIDEN       PIC   N(02).
             07  TBL-MEIKAN       PIC   X(01).
             07  TBL-MEIKNN       PIC   9(08).
             07  TBL-MEISIR       PIC   9(08).
             07  TBL-MEISRN       PIC   N(10).
             07  TBL-MEIJAN       PIC   X(13).
             07  TBL-MEISYO       PIC   X(19).
             07  TBL-MEITAN       PIC   X(06).
             07  TBL-MEIHAC       PIC  S9(09).
             07  TBL-MEINYK       PIC  S9(09).
             07  TBL-MEIZAN       PIC  S9(09).
             07  TBL-MEINKN       PIC   X(01).
             07  TBL-MEINM1       PIC   N(15).
             07  TBL-MEINM2       PIC   N(15).
             07  TBL-MEISOK       PIC   X(02).
             07  TBL-MEISCD       PIC   X(08).
             07  TBL-MEIHN1       PIC   X(05).
             07  TBL-MEIHN2       PIC   X(02).
             07  TBL-MEIHN3       PIC   X(01).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  SFR-ERR           PIC N(15) VALUE
                        NC"社内振替情報ファイルエラー".
     03  SOK-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
     03  JYO-ERR           PIC N(15) VALUE
                        NC"条件ファイルエラー".
     03  SHI-ERR           PIC N(15) VALUE
                        NC"仕入先マスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
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
*明細照会用パラメタ
 01  PA-MEISAI.
     03  PA-DENKU                 PIC  X(02).
     03  PA-NENDO                 PIC  9(04).
     03  PA-SEASON                PIC  X(02).
     03  PA-SOKCD                 PIC  X(02).
     03  PA-SYOCD                 PIC  X(08).
     03  PA-HINTAN1               PIC  X(05).
     03  PA-HINTAN2               PIC  X(02).
     03  PA-HINTAN3               PIC  X(01).
     03  PA-SIRCD                 PIC  9(08).
     03  PA-HACGK                 PIC S9(09).
     03  PA-NYKGK                 PIC S9(09).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 LINKAGE               SECTION.
 01  LINK-BUMON            PIC X(04).
 01  LINK-TANCD            PIC X(02).
 01  LINK-SOKCD            PIC X(02).
 01  LINK-DSOKCD           PIC X(02).
**************************************************************
 PROCEDURE             DIVISION
                          USING  LINK-BUMON
                                 LINK-TANCD
                                 LINK-SOKCD
                                 LINK-DSOKCD.
**************************************************************
 DECLARATIVES.
 JI6-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SFRHEDF.
     MOVE        SFR-ST    TO        E-ST.
     MOVE       "SFRHEDL4" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SFR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST    TO        E-ST.
     MOVE       "ZSOKMS1 " TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE       "JYOKEN1"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SHI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSHIMS.
     MOVE        SHI-ST    TO        E-ST.
     MOVE       "ZSHIMS1"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SHI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE       "DSPFILE"  TO        E-FILE.
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
*
 CONTROL-EXIT.
     STOP  RUN.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT     ZSOKMS  HJYOKEN  ZSHIMS.
     OPEN      I-O       DSPFILE.
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
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM   INIT-DSP-SEC.
     MOVE    "1"                  TO   PSW.
*頁セット
     MOVE     1                   TO   P-CNT.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*ヘッダ入力
         WHEN      "1"       PERFORM   DSP-HEAD-SEC
*明細入力
         WHEN      "2"       PERFORM   DSP-BODY-SEC
*確認入力
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
*明細照会番号入力
         WHEN      "4"       PERFORM   DSP-MEISAI-SEC
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
     CLOSE   ZSOKMS  HJYOKEN  ZSHIMS.
     CLOSE   DSPFILE.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                            1.1      *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
 INITD001.
*画面の初期化
     MOVE    SPACE                TO   DSP-FFU30601.
 INITD003.
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
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時刻取得
     ACCEPT    WK-TIME    FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*システム日付・時間転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
     MOVE    HEN-TIME(1:5)        TO   DSP-STIME.
*明細頁
     MOVE    ZERO                 TO   MEIALLCNT.
 INITD004.
*ＰＧＩＤ・ＦＭＴＩＤセット
     MOVE    SET-PGID             TO   DSP-PGID.
     MOVE    SET-FORMID           TO   DSP-FORMID.
*倉庫コードパラメタより倉庫コード入力チェック
     IF       LINK-DSOKCD  =  "01" OR "88"
              MOVE    " "         TO   EDIT-STATUS OF DSP-HSOKCD
     ELSE
              MOVE  LINK-SOKCD    TO   SOK-F01 DSP-HSOKCD
              PERFORM ZSOKMS-READ-SEC
              IF  ZSOKMS-INV-FLG = "INV"
                  MOVE      ALL NC"＊" TO   DSP-HSOKNM
              ELSE
                  MOVE      SOK-F02    TO   DSP-HSOKNM
              END-IF
**************プロテクト追加
              MOVE    "X"    TO   EDIT-STATUS OF DSP-HSOKCD
     END-IF.
 INITD005.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
     PERFORM  DSP-BODYCL-SEC.
*
     MOVE     1                   TO   P-CNT.
*
 INIT-DSP-EXIT.
     EXIT.
****************************************************************
*    ヘッダ入力
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE     "DSP-HEAD-SEC"      TO   S-NAME.
*
     MOVE     SPACE               TO   DSP-MAS003.
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
       WHEN   "E000"
               PERFORM  HEAD-CHK-SEC
               IF  ERR-FLG  =  ZERO
                   MOVE  "2"   TO  PSW
                   PERFORM DSP-SYOKI-SEC
               END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
                PERFORM   INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*    ヘッダ項目チェック
****************************************************************
 HEAD-CHK-SEC              SECTION.
     MOVE     "HEAD-CHK-SEC"      TO   S-NAME.
*倉庫ＣＤチェック
     IF  DSP-HSOKCD  =  SPACE
         MOVE SPACE     TO  DSP-HSOKCD
     END-IF.
     MOVE DSP-HSOKCD TO  SOK-F01
     PERFORM ZSOKMS-READ-SEC
     IF  ZSOKMS-INV-FLG = "INV"
         MOVE   2       TO  ERR-FLG
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HSOKCD
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-HSOKCD
     ELSE
         MOVE  SOK-F02  TO  DSP-HSOKNM
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-HSOKCD
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-HSOKCD
     END-IF.
*商品ＣＤチェック
*仕入先チェック
     IF  DSP-HSIRCD  NOT NUMERIC
     OR  DSP-HSIRCD  =   ZERO
         MOVE     ZERO  TO   DSP-HSIRCD
     END-IF.
*
     IF  DSP-HSIRCD  NOT =  ZERO
         MOVE DSP-HSIRCD  TO  SHI-F01
         PERFORM ZSHIMS-READ-SEC
         IF  ZSHIMS-INV-FLG = "INV"
             MOVE   3       TO  ERR-FLG
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HSIRCD
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-HSIRCD
         ELSE
             MOVE  SHI-F02  TO  DSP-HSIRNM
             MOVE  "M"      TO  EDIT-OPTION  OF  DSP-HSIRCD
             MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-HSIRCD
         END-IF
     END-IF.
*年度
     IF  DSP-HNENDO  NOT  NUMERIC
     OR  DSP-HNENDO  =  ZERO
         MOVE   ZERO    TO  DSP-HNENDO
     END-IF.
*シーズン
     IF  DSP-HSEASN  =  SPACE
         MOVE   SPACE   TO  DSP-HSEASN
     END-IF.
*伝票区分
     IF  DSP-HDENKU  =  SPACE
         MOVE   SPACE   TO  DSP-HDENKU
     ELSE
         MOVE 1           TO  JYO-F01
         MOVE DSP-HDENKU  TO  JYO-F02
         PERFORM HJYOKEN-READ-SEC
         IF  HJYOKEN-INV-FLG = "INV"
             IF  ERR-FLG  =  ZERO
                 MOVE   4       TO  ERR-FLG
             END-IF
             MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HDENKU
             MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-HDENKU
         ELSE
             MOVE  JYO-F03  TO  DSP-DENKUN
             MOVE  "M"      TO  EDIT-OPTION  OF  DSP-HDENKU
             MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-HDENKU
         END-IF
     END-IF.
*完納区分
     IF  DSP-HKANNO =  SPACE  OR  "1"  OR  "2"
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-HKANNO
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-HKANNO
     ELSE
         IF  ERR-FLG  =  ZERO
             MOVE   5       TO  ERR-FLG
         END-IF
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HKANNO
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-HKANNO
     END-IF.
*エラーがあった場合はＥＸＩＴへ
     IF  ERR-FLG  NOT =  ZERO
         GO              TO      HEAD-CHK-EXIT
     END-IF.
*ファイルよりワークにセット
     PERFORM SFRHEDL4-SET-SEC.
*明細存在チェック判定
     IF  MEISAI-CNT  =  ZERO
         MOVE   6       TO  ERR-FLG
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HSOKCD
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HSYOCD
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HHINT1
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HHINT2
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HHINT3
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HSIRCD
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HNENDO
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HSEASN
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HDENKU
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HKANNO
         IF LINK-DSOKCD = "01" OR "88"
            MOVE "C"    TO  EDIT-CURSOR  OF  DSP-HSOKCD
         ELSE
            MOVE "C"    TO  EDIT-CURSOR  OF  DSP-HSYOCD
         END-IF
     ELSE
         MOVE    1      TO  P-CNT
         PERFORM WORK-DSP-SEC
     END-IF.
*
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*    出荷状況管理ファイル読込（ＫＥＹ＝６）
****************************************************************
 SFRHEDL4-SET-SEC       SECTION.
*
     MOVE     "SFRHEDL4-SET-SEC"   TO   S-NAME.
*
     MOVE      SPACE               TO   SFRHEDF-END-FLG.
*ファイルをオープンする。
     OPEN  INPUT  SFRHEDF.
*ワーク領域初期化
     MOVE      SPACE               TO   TABLE-AREA.
     INITIALIZE                         TABLE-AREA.
     MOVE      ZERO                TO   MEISAI-CNT  MEIALLCNT.
*社内振替情報ファイルスタート
     PERFORM SFRHEDL4-START-SEC.
*社内振替情報ファイル読込
     IF  SFRHEDF-END-FLG  NOT =  "END"
         PERFORM SFRHEDL4-READ-SEC
     END-IF.
*ファイルよりワークへセット
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 50
                            OR SFRHEDF-END-FLG = "END"
******   DISPLAY "IX  = " IX  UPON CONS
         PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 4
                            OR SFRHEDF-END-FLG = "END"
******   DISPLAY " IY = " IY  UPON CONS
*          *>年度
             MOVE  SFR-F02            TO  TBL-MEINEN(IX IY)
*          *>シーズン
             MOVE  SFR-F03            TO  TBL-MEISEA(IX IY)
*          *>伝票区分／伝票区分名
             MOVE  SFR-F01            TO  TBL-MEIDEK(IX IY)
             MOVE  1                  TO  JYO-F01
             MOVE  SFR-F01            TO  JYO-F02
             PERFORM HJYOKEN-READ-SEC
             IF   HJYOKEN-INV-FLG = "INV"
                  MOVE ALL NC"＊"     TO  TBL-MEIDEN(IX IY)
             ELSE
                  MOVE JYO-F03        TO  TBL-MEIDEN(IX IY)
             END-IF
*          *>完納区分セット
             MOVE  SFR-F24            TO  TBL-MEIKAN(IX IY)
             MOVE  SFR-F25            TO  TBL-MEIKNN(IX IY)
*          *>仕入先ＣＤ／仕入先名
             MOVE  SFR-F12            TO  TBL-MEISIR(IX IY)
             MOVE  SFR-F12            TO  SHI-F01
             PERFORM ZSHIMS-READ-SEC
             IF   ZSHIMS-INV-FLG = "INV"
                  MOVE ALL NC"＊"     TO  TBL-MEISRN(IX IY)
             ELSE
                  MOVE SHI-F02        TO  TBL-MEISRN(IX IY)
             END-IF
*          *>ＪＡＮＣＤ
             MOVE  SFR-F13            TO  TBL-MEIJAN(IX IY)
*          *>商品情報
             MOVE  SFR-F05            TO  WK-SYOCD1
             MOVE  SFR-F06            TO  WK-SYOCD2
             MOVE  SFR-F07            TO  WK-SYOCD3
             MOVE  SFR-F08            TO  WK-SYOCD4
             MOVE  SPACE              TO  WK-SYOCD-KU1
             MOVE  "-"                TO  WK-SYOCD-KU2
             MOVE  "-"                TO  WK-SYOCD-KU3
             MOVE  WK-SYOCD           TO  TBL-MEISYO(IX IY)
*          *>_番
             MOVE  SFR-F11            TO  TBL-MEITAN(IX IY)
*          *>発注合計
             MOVE  SFR-F15            TO  TBL-MEIHAC(IX IY)
*          *>入荷合計
             MOVE  SFR-F19            TO  TBL-MEINYK(IX IY)
*          *>発注残数
             COMPUTE TBL-MEIZAN(IX IY) = SFR-F15 - SFR-F19
*          *>完納区分
             MOVE  SFR-F24            TO  TBL-MEINKN(IX IY)
*          *>商品名
             MOVE  SFR-F09            TO  TBL-MEINM1(IX IY)
             MOVE  SFR-F10            TO  TBL-MEINM2(IX IY)
*          *>倉庫ＣＤ
             MOVE  SFR-F04            TO  TBL-MEISOK(IX IY)
*          *>サカタ商品情報
             MOVE  SFR-F05            TO  TBL-MEISCD(IX IY)
             MOVE  SFR-F06            TO  TBL-MEIHN1(IX IY)
             MOVE  SFR-F07            TO  TBL-MEIHN2(IX IY)
             MOVE  SFR-F08            TO  TBL-MEIHN3(IX IY)
*
             MOVE    "1"              TO  PAGE-SONZAI-CHK(IX)
             MOVE    IX               TO  MEIALLCNT
*
             ADD      1               TO  MEISAI-CNT
*
             PERFORM SFRHEDL4-READ-SEC
         END-PERFORM
     END-PERFORM.
*
     CLOSE  SFRHEDF.
*
 SFRHEDL4-SET-EXIT.
     EXIT.
****************************************************************
*    ワークより、画面項目にセット
****************************************************************
 WORK-DSP-SEC              SECTION.
*
     MOVE "WORK-DSP-SEC"     TO   S-NAME.
*明細初期化
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 4
             MOVE  SPACE     TO   DSP-MAS001(IX)
     END-PERFORM.
     MOVE  ZERO              TO   MEISAI-CNT.
*
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 4
          IF TBL-MEINEN(P-CNT IX) NOT = ZERO
             MOVE TBL-MEINEN(P-CNT IX)  TO  DSP-MNENDO(IX)
             MOVE TBL-MEISEA(P-CNT IX)  TO  DSP-MSEASN(IX)
             MOVE TBL-MEIDEK(P-CNT IX)  TO  DSP-MDENKU(IX)
             MOVE TBL-MEIDEN(P-CNT IX)  TO  DSP-MDENKN(IX)
             MOVE TBL-MEIKAN(P-CNT IX)  TO  DSP-MKANNO(IX)
             MOVE TBL-MEIKNN(P-CNT IX)  TO  DSP-MKANDT(IX)
             MOVE TBL-MEISIR(P-CNT IX)  TO  DSP-MSIRCD(IX)
             MOVE TBL-MEISRN(P-CNT IX)  TO  DSP-MSIRNM(IX)
             MOVE TBL-MEISYO(P-CNT IX)  TO  DSP-MSYOCD(IX)
             MOVE TBL-MEITAN(P-CNT IX)  TO  DSP-MTANBN(IX)
             MOVE TBL-MEIHAC(P-CNT IX)  TO  DSP-MHACGK(IX)
             MOVE TBL-MEINYK(P-CNT IX)  TO  DSP-MNYKGK(IX)
             MOVE TBL-MEIZAN(P-CNT IX)  TO  DSP-MHACZN(IX)
             MOVE TBL-MEINKN(P-CNT IX)  TO  DSP-MKANKB(IX)
             MOVE TBL-MEINM1(P-CNT IX)  TO  DSP-MSYON1(IX)
             MOVE TBL-MEINM2(P-CNT IX)  TO  DSP-MSYON2(IX)
          ELSE
             MOVE "X" TO   EDIT-STATUS OF DSP-MKANKB(IX)
          END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*    画面よりワークにセット
****************************************************************
 DSP-TBL-SEC               SECTION.
*
     MOVE "DSP-TBL-SEC"      TO   S-NAME.
*
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 4
         IF  DSP-MNENDO(IX) NOT =  ZERO
             MOVE  DSP-MKANKB(IX)       TO  TBL-MEINKN(P-CNT IX)
         END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             明細入力( PSW = 4 )                     2.4      *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"      TO   S-NAME.
*
     MOVE      MEIALLCNT          TO   DSP-HPAGEA.
     MOVE      P-CNT              TO   DSP-HPAGE.
     MOVE      "/"                TO   DSP-HPAGEK.
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   BODY-CHK-SEC
                IF  ERR-FLG  =  ZERO
                    PERFORM     DSP-TBL-SEC
                    PERFORM     WORK-DSP-SEC
                    MOVE  "3"   TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
                PERFORM     INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻り
         WHEN   "F006"
                MOVE    "1"      TO    PSW
                PERFORM DSP-BODYCL-SEC
*明細照会番号入力
         WHEN   "F007"
                MOVE    "4"      TO    PSW
*前頁
         WHEN   "F011"
             PERFORM   BODY-CHK-SEC
             IF   ERR-FLG  =  ZERO
                  PERFORM  DSP-TBL-SEC
** 頁チェック
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  7  TO  ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT - 1
                        PERFORM WORK-DSP-SEC
                END-IF
             END-IF
*次頁
         WHEN   "F012"
             PERFORM   BODY-CHK-SEC
             IF   ERR-FLG  =  ZERO
                  PERFORM  DSP-TBL-SEC
** 頁チェック
                MOVE    P-CNT   TO      S-CNT
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > MAX-PGCNT
                        MOVE   8   TO   ERR-FLG
                ELSE
                  IF    PAGE-SONZAI-CHK(C-CNT) = SPACE
                        MOVE   8     TO    ERR-FLG
                  ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        PERFORM WORK-DSP-SEC
                  END-IF
                END-IF
             END-IF
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細部チェック                          2.4.1    *
****************************************************************
 BODY-CHK-SEC             SECTION.
*
     MOVE     "BODY-CHK-SEC"     TO   S-NAME.
*
     MOVE      ZERO              TO   Y-CNT.
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MEISAI-CNT
*
     IF  DSP-MKANKB(Y)  =  SPACE  OR  "1"
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-MKANKB(Y)
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-MKANKB(Y)
     ELSE
         MOVE  5        TO  ERR-FLG
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-MKANKB(Y)
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-MKANKB(Y)
     END-IF
     END-PERFORM.
*
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*    明細照会番号入力(PSW=4)
****************************************************************
 DSP-MEISAI-SEC        SECTION.
     MOVE     "DSP-MEISAI-SEC"    TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   MEISAI-CHK-SEC
                IF  ERR-FLG  =  ZERO
                    MOVE SPACE  TO   DSP-MAS002
                    MOVE  "2"   TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
                PERFORM     INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻り
         WHEN   "F006"
                MOVE    "2"      TO    PSW
                PERFORM DSP-BODYCL-SEC
     END-EVALUATE.
*
 DSP-MEISAI-EXIT.
     EXIT.
****************************************************************
*             明細部チェック                          2.4.1    *
****************************************************************
 MEISAI-CHK-SEC           SECTION.
*
     MOVE     "MEISAI-CHK-SEC"   TO   S-NAME.
*明細照会番号チェック
     IF  DSP-MEINO  =  ZERO
     OR  DSP-MEINO  NOT NUMERIC
         MOVE   ZERO    TO  DSP-MEINO
     END-IF.
*
     IF  DSP-MEINO  =  1 OR 2 OR 3 OR 4
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-MEINO
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-MEINO
     ELSE
         MOVE   9       TO  ERR-FLG
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-MEINO
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-MEINO
     END-IF.
*明細データ存在チェック
*****DISPLAY "TBL-MEINEN  = " TBL-MEINEN(P-CNT DSP-MEINO)
*****                         UPON CONS.
     IF  TBL-MEINEN(P-CNT DSP-MEINO) > ZERO
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-MEINO
         MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-MEINO
     ELSE
         IF  ERR-FLG  =  ZERO
             MOVE   10      TO  ERR-FLG
         END-IF
         MOVE  "R"      TO  EDIT-OPTION  OF  DSP-MEINO
         MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-MEINO
     END-IF.
*
*****DISPLAY "ERR-FLG = " ERR-FLG  UPON CONS.
     IF   ERR-FLG  NOT =  ZERO
          GO              TO     MEISAI-CHK-EXIT
     END-IF.
*
     MOVE  SPACE          TO     PA-MEISAI.
     INITIALIZE                  PA-MEISAI.
     MOVE  TBL-MEIDEK(P-CNT DSP-MEINO)     TO     PA-DENKU.
     MOVE  TBL-MEINEN(P-CNT DSP-MEINO)     TO     PA-NENDO.
     MOVE  TBL-MEISEA(P-CNT DSP-MEINO)     TO     PA-SEASON.
     MOVE  TBL-MEISOK(P-CNT DSP-MEINO)     TO     PA-SOKCD.
     MOVE  TBL-MEISYO(P-CNT DSP-MEINO)     TO     PA-SYOCD.
     MOVE  TBL-MEIHN1(P-CNT DSP-MEINO)     TO     PA-HINTAN1.
     MOVE  TBL-MEIHN2(P-CNT DSP-MEINO)     TO     PA-HINTAN2.
     MOVE  TBL-MEIHN3(P-CNT DSP-MEINO)     TO     PA-HINTAN3.
     MOVE  TBL-MEISIR(P-CNT DSP-MEINO)     TO     PA-SIRCD.
     MOVE  TBL-MEIHAC(P-CNT DSP-MEINO)     TO     PA-HACGK.
     MOVE  TBL-MEINYK(P-CNT DSP-MEINO)     TO     PA-NYKGK.
*
*****CALL "SFU3070I"  USING PA-MEISAI.
     CALL "SFU3070I"  USING                       PA-DENKU
                                                  PA-NENDO
                                                  PA-SEASON
                                                  PA-SOKCD
                                                  PA-SYOCD
                                                  PA-HINTAN1
                                                  PA-HINTAN2
                                                  PA-HINTAN3
                                                  PA-SIRCD
                                                  PA-HACGK
                                                  PA-NYKGK.
*
*全画面クリア（呼んだPGの画面表示をクリアする)
     MOVE  "CL"           TO  DSP-PRO.
     MOVE  "SCREEN"       TO  DSP-GRP.
     MOVE  "FFU30601"     TO  DSP-FMT.
     WRITE  DSP-FFU30601.
*通常出力指定に戻す。
     MOVE  SPACE          TO  DSP-PRO.
*
 MEISAI-CHK-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 5 ）             2.5
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
                PERFORM  MENT-WRITE-SEC
                PERFORM  DSP-BODYCL-SEC
                PERFORM  INIT-DSP-SEC
                MOVE     1       TO   P-CNT
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                PERFORM  INIT-DSP-SEC
                MOVE    "1"      TO   PSW
                MOVE     1       TO   P-CNT
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻り
         WHEN   "F006"
                MOVE    "2"      TO   PSW
****************PERFORM DSP-BODYCL-SEC
*前頁
         WHEN   "F011"
** 頁チェック
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  7  TO  ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT - 1
                        PERFORM WORK-DSP-SEC
                        MOVE    "2"      TO   PSW
                END-IF
*次頁
         WHEN   "F012"
** 頁チェック
                MOVE    P-CNT   TO      S-CNT
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > MAX-PGCNT
                        MOVE   8   TO   ERR-FLG
                ELSE
                  IF    PAGE-SONZAI-CHK(C-CNT) = SPACE
                        MOVE   8     TO    ERR-FLG
                  ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        PERFORM WORK-DSP-SEC
                        MOVE    "2"      TO   PSW
                  END-IF
                END-IF
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
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
         WHEN   "1"
           MOVE    PF-MSG-R(1)        TO   DSP-PFMSG
         WHEN   "2"
           MOVE    PF-MSG-R(2)        TO   DSP-PFMSG
         WHEN   "3"
           MOVE    PF-MSG-R(3)        TO   DSP-PFMSG
         WHEN   "4"
           MOVE    PF-MSG-R(4)        TO   DSP-PFMSG
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"          TO   DSP-GRP.
     MOVE    "FFU30601"        TO   DSP-FMT.
     WRITE    DSP-FFU30601.
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
*ヘッダ入力
         WHEN   "1"
                MOVE    "HED001"  TO   DSP-GRP
*明細入力
         WHEN   "2"
                MOVE    "MES001"  TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
*明細照会番号入力
         WHEN   "4"
                MOVE    "MEIGRP"  TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FFU30601"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
     MOVE    ZERO                 TO   ERR-FLG.
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
***  倉庫ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-HSOKCD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HSOKCD.
***  サカタ商品ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-HSYOCD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HSYOCD.
     MOVE "M"   TO EDIT-OPTION OF DSP-HHINT1.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HHINT1.
     MOVE "M"   TO EDIT-OPTION OF DSP-HHINT2.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HHINT2.
     MOVE "M"   TO EDIT-OPTION OF DSP-HHINT3.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HHINT3.
***  仕入先ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-HSIRCD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HSIRCD.
***  年度
     MOVE "M"   TO EDIT-OPTION OF DSP-HNENDO.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HNENDO.
***  シーズン
     MOVE "M"   TO EDIT-OPTION OF DSP-HSEASN.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HSEASN.
***  伝票区分
     MOVE "M"   TO EDIT-OPTION OF DSP-HDENKU.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HDENKU.
***  完納区分
     MOVE "M"   TO EDIT-OPTION OF DSP-HKANNO.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HKANNO.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*         明細画面制御項目初期化                               *
****************************************************************
 DSP-BODYCL-SEC         SECTION.
     MOVE     "DSP-BODYCL-SEC"    TO   S-NAME.
     MOVE     ZERO                TO   MEIALLCNT.
     PERFORM VARYING Y FROM 1 BY 1 UNTIL  Y  >  MAX-LNCNT
*********MOVE    SPACE    TO  DSP-MAS001(Y)
*リバース，カーソルパーク解除
***  明細完納区分
         MOVE  "M"      TO  EDIT-COLOR   OF  DSP-MKANKB(Y)
         MOVE  "M"      TO  EDIT-OPTION  OF  DSP-MKANKB(Y)
     END-PERFORM.
*
 DSP-BODYCL-EXIT.
     EXIT.
****************************************************************
*             メンテデータ書き込み                   2.5.1     *
****************************************************************
 MENT-WRITE-SEC      SECTION.
*
     MOVE     "MENT-WRITE-SEC"  TO   S-NAME.
*
     OPEN   I-O  SFRHEDF.
*ワークテーブル更新
     PERFORM VARYING X FROM 1 BY 1 UNTIL  X  >  MAX-PGCNT
        PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > MAX-LNCNT
           IF  TBL-MEINEN(X Y) NOT = ZERO
               MOVE  TBL-MEISOK(X Y)  TO  SFR-F04
               MOVE  TBL-MEIDEK(X Y)  TO  SFR-F01
               MOVE  TBL-MEINEN(X Y)  TO  SFR-F02
               MOVE  TBL-MEISEA(X Y)  TO  SFR-F03
               MOVE  TBL-MEISCD(X Y)  TO  SFR-F05
               MOVE  TBL-MEIHN1(X Y)  TO  SFR-F06
               MOVE  TBL-MEIHN2(X Y)  TO  SFR-F07
               MOVE  TBL-MEIHN3(X Y)  TO  SFR-F08
               PERFORM SFRHEDF-READ-SEC
               IF  SFRHEDF-INV-FLG  =  "INV"
                   DISPLAY NC"＃＃更新エラー＃＃" UPON CONS
               ELSE
                   IF  SFR-F24  =  SPACE
                   AND TBL-MEINKN(X Y)  =  "1"
                       MOVE  "1"          TO  SFR-F24
                       MOVE  DATE-AREA    TO  SFR-F25
                       MOVE  LINK-BUMON   TO  SFR-F26  SFR-F96
                       MOVE  LINK-TANCD   TO  SFR-F27  SFR-F97
***********************システム時刻取得
                       ACCEPT    WK-TIME    FROM   TIME
***********************システム日付・時間転送
                       MOVE  DATE-AREA    TO  SFR-F98
                       MOVE  WK-TIME(1:6) TO  SFR-F99
                   ELSE
                       IF   SFR-F24  =  "1"
                       AND  TBL-MEINKN(X Y)  =  SPACE
                            MOVE  SPACE   TO  SFR-F24
                            MOVE  ZERO    TO  SFR-F25
                            MOVE  SPACE   TO  SFR-F26  SFR-F96
                            MOVE  SPACE   TO  SFR-F27  SFR-F97
****************************システム時刻取得
                            ACCEPT    WK-TIME    FROM   TIME
****************************システム日付・時間転送
                            MOVE  DATE-AREA    TO  SFR-F98
                            MOVE  WK-TIME(1:6) TO  SFR-F99
                       END-IF
                   END-IF
                   REWRITE  SFR-REC
               END-IF
           END-IF
        END-PERFORM
     END-PERFORM.
*
     CLOSE SFRHEDF.
*
 MENT-WRITE-EXIT.
     EXIT.
****************************************************************
*    社内振替情報ファイルスタート
****************************************************************
 SFRHEDL4-START-SEC    SECTION.
*
     MOVE    "SFRHEDL4-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   SFR-REC.
     INITIALIZE                        SFR-REC.
*
     MOVE     DSP-HSOKCD          TO   SFR-F04.
     MOVE     DSP-HDENKU          TO   SFR-F01.
     MOVE     DSP-HNENDO          TO   SFR-F02.
     MOVE     DSP-HSEASN          TO   SFR-F03.
     MOVE     DSP-HSYOCD          TO   SFR-F05.
     MOVE     DSP-HHINT1          TO   SFR-F06.
     MOVE     DSP-HHINT2          TO   SFR-F07.
     MOVE     DSP-HHINT3          TO   SFR-F08.
*
     START  SFRHEDF KEY IS >=  SFR-F04  SFR-F01  SFR-F02
                               SFR-F03  SFR-F05  SFR-F06
                               SFR-F07  SFR-F08
            INVALID
            MOVE  "END"           TO   SFRHEDF-END-FLG
     END-START.
*
 SFRHEDL4-START-EXIT.
     EXIT.
****************************************************************
*    社内振替情報ファイル読込
****************************************************************
 SFRHEDL4-READ-SEC     SECTION.
*
     MOVE     "SFRHEDL4-READ-SEC" TO   S-NAME.
*
 SFRHEDL4-010.
     READ   SFRHEDF
            NEXT  AT  END    MOVE  "END"   TO   SFRHEDF-END-FLG
                             GO            TO   SFRHEDL4-READ-EXIT
     END-READ.
 SFRHEDL4-020.
*倉庫ＣＤチェック
*    DISPLAY "DSP-HSOKCD = " DSP-HSOKCD UPON CONS.
     IF     SFR-F04 NOT = DSP-HSOKCD
*******     DISPLAY "SFR-F04 = " SFR-F04 UPON CONS
            MOVE  "END"   TO   SFRHEDF-END-FLG
            GO            TO   SFRHEDL4-READ-EXIT
     END-IF.
 SFRHEDL4-030.
*伝票区分
     IF     DSP-HDENKU  NOT =  SPACE
            IF  DSP-HDENKU  NOT =   SFR-F01
                GO        TO   SFRHEDL4-READ-SEC
            END-IF
     END-IF.
 SFRHEDL4-040.
*年度
     IF     DSP-HNENDO  NOT =  ZERO
            IF  DSP-HNENDO  NOT = SFR-F02
                GO        TO   SFRHEDL4-READ-SEC
            END-IF
     END-IF.
 SFRHEDL4-050.
*シーズン
     IF     DSP-HSEASN  NOT =  SPACE
            IF  DSP-HSEASN  NOT = SFR-F03
                GO        TO   SFRHEDL4-READ-SEC
            END-IF
     END-IF.
 SFRHEDL4-055.
*サカタ商品ＣＤ
     IF     DSP-HSYOCD  NOT =  SPACE
            IF  DSP-HSYOCD  =  SFR-F05
            AND DSP-HHINT1  =  SFR-F06
            AND DSP-HHINT2  =  SFR-F07
            AND DSP-HHINT3  =  SFR-F08
                CONTINUE
            ELSE
                GO        TO   SFRHEDL4-READ-SEC
            END-IF
     END-IF.
 SFRHEDL4-060.
*仕入先ＣＤチェック
     IF     DSP-HSIRCD  NOT =  ZERO
            IF  DSP-HSIRCD  NOT = SFR-F12
                GO        TO   SFRHEDL4-READ-SEC
            END-IF
     END-IF.
 SFRHEDL4-070.
*完納区分チェック
     IF     DSP-HKANNO  NOT =  SPACE
            IF  DSP-HKANNO  =  "1"
                IF  SFR-F24 NOT = "1"
                    GO    TO   SFRHEDL4-READ-SEC
                END-IF
            END-IF
            IF  DSP-HKANNO  =  "2"
                IF  SFR-F24 NOT = SPACE
                    GO    TO   SFRHEDL4-READ-SEC
                END-IF
            END-IF
     END-IF.
*
 SFRHEDL4-READ-EXIT.
     EXIT.
****************************************************************
*    社内振替情報ファイル読込　ＲＡＮＤＯＭ
****************************************************************
 SFRHEDF-READ-SEC      SECTION.
*
     MOVE     "SFRHEDF-READ-SEC"  TO   S-NAME.
*
     READ   SFRHEDF
            INVALID     MOVE  "INV"   TO   SFRHEDF-INV-FLG
            NOT INVALID MOVE  SPACE   TO   SFRHEDF-INV-FLG
     END-READ.
*
 SFRHEDF-READ-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ読込
****************************************************************
 ZSOKMS-READ-SEC      SECTION.
*
     MOVE     "ZSOKMS-READ-SEC"   TO   S-NAME.

     READ   ZSOKMS
            INVALID     MOVE  "INV"   TO   ZSOKMS-INV-FLG
            NOT INVALID MOVE  SPACE   TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル読込
****************************************************************
 HJYOKEN-READ-SEC      SECTION.
*
     MOVE     "HJYOKEN-READ-SEC"  TO   S-NAME.
*
     READ   HJYOKEN
            INVALID     MOVE  "INV"   TO   HJYOKEN-INV-FLG
            NOT INVALID MOVE  SPACE   TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*    仕入先マスタ
****************************************************************
 ZSHIMS-READ-SEC       SECTION.
*
     MOVE     "ZSHIMS-READ-SEC"   TO   S-NAME.
*
     READ   ZSHIMS
            INVALID     MOVE  "INV"   TO   ZSHIMS-INV-FLG
            NOT INVALID MOVE  SPACE   TO   ZSHIMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SFU3060I >*********************

```
