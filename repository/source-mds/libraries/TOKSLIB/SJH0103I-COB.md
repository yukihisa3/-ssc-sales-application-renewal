# SJH0103I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH0103I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　基幹システム改善　　　　　　　　　
*    業務名　　　　　　　：　日次更新条件マスタ　　　　
*    モジュール名　　　　：　日次更新条件マスタ保守　　　　
*    作成日／作成者　　　：　2005/12/15   NAV
*    変更概要　　　　　　：　実行区分が空白の場合、明細項目を　
*                            初期化する。　　　　　　　　　　　
*    変更履歴      　　　：
*      2010/04/27  NAV
*      2011/10/12  飯田/NAV  基幹サーバ統合・業務改善
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJH0103I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         05/12/15.
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
*日次更新条件マスタ　　　
     SELECT  JHMNITF   ASSIGN    TO        JHMNITL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       NIT-F01
                       FILE      STATUS    NIT-ST.
* 2011/10/12,S  S.I/NAV
*条件ファイル
     SELECT  HJYOKEN   ASSIGN    TO        JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY
                        JYK-F01
                        JYK-F02
                       FILE      STATUS    JYK-ST.
* 2011/10/12,E  S.I/NAV
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
*    FILE = 日次更新条件マスタ　　                             *
****************************************************************
 FD  JHMNITF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMNITF   OF   XFDLIB
                       JOINING   NIT       AS   PREFIX.
* 2011/10/12,S  S.I/NAV
****************************************************************
*    FILE = 条件ファイル                                       *
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYK       AS   PREFIX.
* 2011/10/12,E  S.I/NAV
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJH01031  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  NIT-ST                   PIC  X(02).
* 2011/10/12,S  S.I/NAV
     03  JYK-ST                   PIC  X(02).
* 2011/10/12,E  S.I/NAV
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  EOF-FLG                  PIC  X(01)  VALUE  SPACE.
     03  NOU-FLG                  PIC  9(01)  VALUE  ZERO.
* 2011/10/12,S  S.I/NAV
     03  FG-JHMNITF-END           PIC  X(03).
     03  FG-HJYOKEN-INV           PIC  9(01).
     03  FG-TBLSET-OVR            PIC  9(01).
* 2011/10/12,E  S.I/NAV
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.

     03  WK-HIDUKE                PIC  9(02)  VALUE  ZERO.
     03  WK-SYSDATE.
         05  WK-SYSYYYY           PIC  9(04)  VALUE  ZERO.
         05  WK-SYSMM             PIC  9(02)  VALUE  ZERO.
         05  WK-SYSDD             PIC  9(02)  VALUE  ZERO.
     03  REC-CNT                  PIC  9(03)  VALUE  ZERO.
     03  PAGE-CNT                 PIC  9(03)  VALUE  ZERO.
     03  WK-JKTIME                PIC  9(04).
     03  WK-JKTIME-R  REDEFINES   WK-JKTIME.
         05  WK-JKTIME-HH         PIC  9(02).
         05  WK-JKTIME-MM         PIC  9(02).
* 2011/10/12,S  S.I/NAV
     03  WK-SELST-BI              PIC  9(08).
     03  IX-PAGE                  PIC  9(03).
     03  IX-PAGEMAX               PIC  9(03).
     03  IX-GYO                   PIC  9(03).
     03  IX-TBL                   PIC  9(03).
     03  IX-TBLMAX                PIC  9(03).
     03  WK-SET-NISSU-MAX         PIC  9(03).
*2017/02/07 NAV ST
     03  WK-NOUDT-KIJYUN          PIC  9(03).
     03  WK-NOUDT-KIJYUN-BI       PIC  9(08).
*2017/02/07 NAV ED
     03  WK-AMARI                 PIC  9(03).
     03  CT-IN                    PIC  9(04).

* 2011/10/12,E  S.I/NAV

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
 01  WK-JIKAN.
     03  WK-HH                    PIC  9(02)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
*
* 2011/10/12,S  S.I/NAV
 01  TB-GAMEN.
     03  TB-GAMEN-G.
       05  TB-GAMEN-G2  OCCURS 7.
         07  TB-GAMEN-G3  OCCURS 15.
           09  TB-NIT-F01   PIC  9(08). *>日次更新日付
           09  TB-NIT-F09   PIC  9(01). *>曜日区分
           09  TB-NIT-F02   PIC  X(01). *>実行区分
           09  TB-NIT-F10   PIC  X(01). *>月次区分
           09  TB-NIT-F03   PIC  9(04). *>実行時間
           09  TB-NIT-F05   PIC  9(08). *>日次更新・発注日
           09  TB-NIT-F06   PIC  9(08). *>日次更新・納入日
           09  TB-NIT-F07   PIC  9(08). *>日次更新・出荷日
           09  TB-NIT-F08   PIC  X(01). *>実行結果
     03  TB-GAMEN-GR  REDEFINES TB-GAMEN-G.
       05  TB-GAMEN-G2R  OCCURS 105.
         07  TB-NIT-F01R    PIC  9(08). *>日次更新日付
         07  TB-NIT-F09R    PIC  9(01). *>曜日区分
         07  TB-NIT-F02R    PIC  X(01). *>実行区分
         07  TB-NIT-F10R    PIC  X(01). *>月次区分
         07  TB-NIT-F03R    PIC  9(04). *>実行時間
         07  TB-NIT-F05R    PIC  9(08). *>日次更新・発注日
         07  TB-NIT-F06R    PIC  9(08). *>日次更新・納入日
         07  TB-NIT-F07R    PIC  9(08). *>日次更新・出荷日
         07  TB-NIT-F08R    PIC  X(01). *>実行結果
* 2011/10/12,E  S.I/NAV
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了　_前頁　_次頁".
     03  PF-MSG2.
       05  FILLER           PIC  N(30)  VALUE
       NC"_取消　_終了　_項目戻し".
 01  PF-MSG-AREA-R  REDEFINES PF-MSG-AREA.
     03  PF-MSG-R           PIC  N(30)  OCCURS  2.

*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
       05  FILLER           PIC  N(20)  VALUE
       NC"実行区分を入力してください".
     03  ERR-MSG2.
       05  FILLER           PIC  N(20)  VALUE
       NC"キーが無効です".
     03  ERR-MSG3.
       05  FILLER           PIC  N(20)  VALUE
       NC"実行時間を入力してください".
     03  ERR-MSG4.
       05  FILLER           PIC  N(20)  VALUE
       NC"納品日を入力してください".
     03  ERR-MSG5.
       05  FILLER           PIC  N(20)  VALUE
       NC"発注日論理エラー".
     03  ERR-MSG6.
       05  FILLER           PIC  N(20)  VALUE
       NC"納品日論理エラー".
     03  ERR-MSG7.
       05  FILLER           PIC  N(20)  VALUE
       NC"出荷日論理エラー".
     03  ERR-MSG8.
       05  FILLER           PIC  N(20)  VALUE
       NC"実行時間論理エラー".
     03  ERR-MSG9.
       05  FILLER           PIC  N(20)  VALUE
       NC"出荷日は納品日以降には指定できません".
     03  ERR-MSG10.
       05  FILLER           PIC  N(20)  VALUE
       NC"納品日を確認してください".
     03  ERR-MSG11.
       05  FILLER           PIC  N(20)  VALUE
       NC"実行区分に誤りがあります".
     03  ERR-MSG12.
       05  FILLER           PIC  N(20)  VALUE
       NC"月次区分に誤りがあります".
     03  ERR-MSG13.
       05  FILLER           PIC  N(20)  VALUE
       NC"納品日は当日以降の日付は指定できません".
     03  ERR-MSG14.
       05  FILLER           PIC  N(20)  VALUE
       NC"前頁が有りません".
     03  ERR-MSG15.
       05  FILLER           PIC  N(20)  VALUE
       NC"次頁が有りません".
     03  ERR-MSG16.
       05  FILLER           PIC  N(20)  VALUE
       NC"条件ファイルに設定可能日数が未設定です".
     03  ERR-MSG17.
       05  FILLER           PIC  N(20)  VALUE
       NC"対象の日次更新条件マスタが存在しません".
     03  ERR-MSG18.
       05  FILLER           PIC  N(20)  VALUE
       NC"内容を確認しＥＮＴＥＲを押下して下さい".
     03  ERR-MSG19.
       05  FILLER           PIC  N(20)  VALUE
       NC"納品日は実行日付の７日前までが指定可能！".
     03  ERR-MSG20.
       05  FILLER           PIC  N(20)  VALUE
       NC"出荷日は実行日付の７日前までが指定可能！".
 01  ERR-MSG-AREA-R  REDEFINES ERR-MSG-AREA.
     03  ERR-MSG-R          PIC  N(20)  OCCURS 20.

* ファイルエラーメッセージ
 01  FILE-ERR.
     03  FAX-ERR            PIC  N(15)  VALUE
     NC"日時更新条件Ｍエラー".
* 2011/10/12,S  S.I/NAV
     03  JYK-ERR            PIC  N(15)  VALUE
     NC"条件ファイルエラー".
* 2011/10/12,E  S.I/NAV
     03  DSP-ERR            PIC  N(15)  VALUE
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 NIT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMNITF.
     MOVE        NIT-ST    TO        E-ST.
     MOVE        "JHMNITF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     FAX-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
* 2011/10/12,S  S.I/NAV
 JYK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYK-ST    TO        E-ST.
     MOVE        "HJYOKEN" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
* 2011/10/12,E  S.I/NAV
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
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.

* ワークの初期化
* 2011/10/12,S  S.I/NAV
     MOVE  SPACE            TO  WRK-AREA.
     INITIALIZE  WRK-AREA.
* 2011/10/12,E  S.I/NAV

     INITIALIZE  FLG-AREA.

* システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM   DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING  LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.
* 画面表示日付編集
     MOVE  SYS-DATE(1:4)    TO  HEN-DATE-YYYY.
     MOVE  SYS-DATE(5:2)    TO  HEN-DATE-MM.
     MOVE  SYS-DATE(7:2)    TO  HEN-DATE-DD.
* システム時刻取得
     ACCEPT  WK-TIME   FROM TIME.
* 画面表示時刻編集
     MOVE  WK-TIME(1:2)     TO  HEN-TIME-HH.
     MOVE  WK-TIME(3:2)     TO  HEN-TIME-MM.
     MOVE  WK-TIME(5:2)     TO  HEN-TIME-SS.
* ファイルのＯＰＥＮ
     OPEN  I-O    JHMNITF.
* 2011/10/12,S  S.I/NAV
     OPEN  INPUT  HJYOKEN.
* 2011/10/12,E  S.I/NAV
     OPEN  I-O    DSPFILE.
* 初期画面の表示
     MOVE  SPACE            TO  DSP-PRO.
*ヘッド入力へ
     MOVE  "1"              TO  PSW.

 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     EVALUATE  PSW
    *> 画面初期表示
       WHEN  "1"  PERFORM  DSP-INIT-SEC
    *> 明細入力
       WHEN  "2"  PERFORM  DSP-MEIS-SEC
    *> 確認入力
       WHEN  "3"  PERFORM  DSP-KAKU-SEC
    *> 以外
       WHEN  OTHER
         CONTINUE

     END-EVALUATE.

 MAIN-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE  "DSP-INIT-SEC"   TO  S-NAME.
*  初期画面の表示
     MOVE  SPACE            TO  DSP-PRO.

* 2011/10/12,S  S.I/NAV
*    日次更新条件マスタをテーブルに退避する。
     PERFORM  TBL-SET-SEC.
*    テーブルから明細項目セット
     MOVE  1                TO  IX-PAGE.
     PERFORM  GAMEN-EDT-SEC.
* 2011/10/12,E  S.I/NAV

* バッチ_入力へ
     MOVE  "2"              TO  PSW.

 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*    テーブル設定処理  2011/10/12 追加
****************************************************************
 TBL-SET-SEC          SECTION.
     MOVE  "TBL-SET-SEC"    TO  S-NAME.

* 処理日付を求める
     ACCEPT  WK-DATE   FROM  DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.
     MOVE  SYS-DATE         TO  WK-SELST-BI.

* 設定可能日数を条件マスタより取得する。
     MOVE   11             TO  JYK-F01.
     MOVE  "01"            TO  JYK-F02.

     READ  HJYOKEN  KEY IS JYK-F01
                           JYK-F02
       INVALID
         MOVE  1            TO  FG-HJYOKEN-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HJYOKEN-INV
     END-READ.

     IF  FG-HJYOKEN-INV = 1
         IF  ERR-FLG =  ZERO
             MOVE  16       TO  ERR-FLG
         END-IF
         GO TO  TBL-SET-EXIT
     END-IF.

     IF  JYK-F04 = ZERO
         IF  ERR-FLG =  ZERO
             MOVE  16       TO  ERR-FLG
         END-IF
         GO TO  TBL-SET-EXIT
     END-IF.
     MOVE  JYK-F04          TO  WK-SET-NISSU-MAX.
*2017/02/07 NAV ST
     MOVE  JYK-F05          TO  WK-NOUDT-KIJYUN.
*2017/02/07 NAV ED

  *> 設定可能日数の日次更新条件マスタを内部ＴＢＬに
  *> に出力する。
     INITIALIZE  TB-GAMEN.
     MOVE  ZERO             TO  IX-TBL.
     MOVE  ZERO             TO  IX-TBLMAX.
     MOVE  ZERO             TO  FG-TBLSET-OVR.
     MOVE  ZERO             TO  CT-IN.

*  日次更新条件抽出ファイルを読込み内部テーブルに格納する。
     MOVE  WK-SELST-BI      TO  NIT-F01.
     MOVE  LOW-VALUE        TO  FG-JHMNITF-END.
     PERFORM  RD-JHMNITF-SEC.
     IF  FG-JHMNITF-END = "END"
         IF  ERR-FLG =  ZERO
             MOVE  17       TO  ERR-FLG
         END-IF
         GO TO  TBL-SET-EXIT
     END-IF.


     PERFORM  UNTIL  FG-JHMNITF-END = "END"
       PERFORM  TBL-SETB-SEC
       PERFORM  RD-JHMNITF-SEC

     END-PERFORM.

* 最終頁の計算
     MOVE  ZERO             TO  IX-PAGEMAX.
     MOVE  ZERO             TO  WK-AMARI.
     DIVIDE  15   INTO  IX-TBLMAX
       GIVING     IX-PAGEMAX
       REMAINDER  WK-AMARI.

     IF  WK-AMARI NOT = ZERO
         COMPUTE  IX-PAGEMAX = IX-PAGEMAX + 1
     END-IF.

 TBL-SET-EXIT.
     EXIT.
****************************************************************
*  日次条件更新マスタ読込み
****************************************************************
 RD-JHMNITF-SEC        SECTION.
     MOVE  "RD-JHMNITF-SEC"  TO  S-NAME.

     IF  FG-JHMNITF-END = LOW-VALUE
         MOVE  SPACE        TO  FG-JHMNITF-END

         START  JHMNITF  KEY >= NIT-F01
           INVALID  KEY
             MOVE  "END"    TO  FG-JHMNITF-END
             GO TO  RD-JHMNITF-EXIT
         END-START

     END-IF.

     READ  JHMNITF  NEXT
       AT END
         MOVE  "END"        TO  FG-JHMNITF-END
         GO TO  RD-JHMNITF-EXIT
     END-READ.

     ADD  1   TO  CT-IN.

     IF  CT-IN >  WK-SET-NISSU-MAX *> 最大設定回数
         MOVE  "END"        TO  FG-JHMNITF-END
     END-IF.

 RD-JHMNITF-EXIT.
     EXIT.
****************************************************************
*    テーブル設定Ｂ処理  2011/10/12 追加
****************************************************************
 TBL-SETB-SEC          SECTION.
     ADD  1   TO  IX-TBL.
     IF  IX-TBL >= 99
         MOVE  1            TO  FG-TBLSET-OVR
         GO TO  TBL-SETB-EXIT
     END-IF.

     MOVE  NIT-F01  TO  TB-NIT-F01R (IX-TBL). *>日次更新日付
     MOVE  NIT-F09  TO  TB-NIT-F09R (IX-TBL). *>曜日区分
     MOVE  NIT-F02  TO  TB-NIT-F02R (IX-TBL). *>実行区分
     MOVE  NIT-F10  TO  TB-NIT-F10R (IX-TBL). *>月次区分
     MOVE  NIT-F03  TO  TB-NIT-F03R (IX-TBL). *>実行時間
     MOVE  NIT-F05  TO  TB-NIT-F05R (IX-TBL). *>日次更新発注日
     MOVE  NIT-F06  TO  TB-NIT-F06R (IX-TBL). *>日次更新納入日
     MOVE  NIT-F07  TO  TB-NIT-F07R (IX-TBL). *>日次更新出荷日
     MOVE  NIT-F08  TO  TB-NIT-F08R (IX-TBL). *>実行結果
     MOVE  IX-TBL   TO  IX-TBLMAX.

 TBL-SETB-EXIT.
     EXIT.
****************************************************************
*    画面編集処理  2011/10/12 追加
****************************************************************
 GAMEN-EDT-SEC          SECTION.
*  画面の初期化
     MOVE  SPACE            TO  DSP-FJH01031.
     PERFORM  DSP-SYOKI-SEC.
*  システム日付転送
     MOVE  HEN-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  HEN-TIME         TO  DSP-STIME.

     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > 15
       PERFORM  GAMEN-EDTB-SEC

     END-PERFORM.

 GAMEN-EDT-EXIT.
     EXIT.
****************************************************************
*  項目クリア
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE  "DSP-SYOKI-SEC"  TO  S-NAME.

     *> リバース，カーソルパーク解除
     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > 15
       MOVE  "M"    TO  EDIT-OPTION OF DSP-JKKBN  (IX-GYO)
       MOVE  SPACE  TO  EDIT-CURSOR OF DSP-JKKBN  (IX-GYO)
       MOVE  "M"    TO  EDIT-OPTION OF DSP-GJKBN  (IX-GYO)
       MOVE  SPACE  TO  EDIT-CURSOR OF DSP-GJKBN  (IX-GYO)
       MOVE  "M"    TO  EDIT-OPTION OF DSP-JKTIME (IX-GYO)
       MOVE  SPACE  TO  EDIT-CURSOR OF DSP-JKTIME (IX-GYO)
       MOVE  "M"    TO  EDIT-OPTION OF DSP-HACDAY (IX-GYO)
       MOVE  SPACE  TO  EDIT-CURSOR OF DSP-HACDAY (IX-GYO)
       MOVE  "M"    TO  EDIT-OPTION OF DSP-NOUDAY (IX-GYO)
       MOVE  SPACE  TO  EDIT-CURSOR OF DSP-NOUDAY (IX-GYO)
       MOVE  "M"    TO  EDIT-OPTION OF DSP-SYUDAY (IX-GYO)
       MOVE  SPACE  TO  EDIT-CURSOR OF DSP-SYUDAY (IX-GYO)

     END-PERFORM.

 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*    画面編集処理  2011/10/12 追加
****************************************************************
 GAMEN-EDTB-SEC          SECTION.
       IF ((IX-PAGE - 1) * 15) + IX-GYO  <=  IX-TBLMAX
      *> 明細行編集
          CONTINUE
       ELSE
     *> 空白行編集
          MOVE  SPACE       TO  DSP-MAS001 (IX-GYO)
          MOVE  "X"   TO  EDIT-STATUS OF DSP-JKKBN  (IX-GYO)
          MOVE  "X"   TO  EDIT-STATUS OF DSP-GJKBN  (IX-GYO)
          MOVE  "X"   TO  EDIT-STATUS OF DSP-JKTIME (IX-GYO)
          MOVE  "X"   TO  EDIT-STATUS OF DSP-HACDAY (IX-GYO)
          MOVE  "X"   TO  EDIT-STATUS OF DSP-NOUDAY (IX-GYO)
          MOVE  "X"   TO  EDIT-STATUS OF DSP-SYUDAY (IX-GYO)
          GO TO  GAMEN-EDTB-EXIT
       END-IF.

*  日付
     MOVE  TB-NIT-F01 (IX-PAGE IX-GYO)  TO  DSP-DATE (IX-GYO).
*  曜日
     EVALUATE  TB-NIT-F09 (IX-PAGE IX-GYO)
       WHEN    1
         MOVE  NC"月"       TO  DSP-YOUBI (IX-GYO)
       WHEN    2
         MOVE  NC"火"       TO  DSP-YOUBI (IX-GYO)
       WHEN    3
         MOVE  NC"水"       TO  DSP-YOUBI (IX-GYO)
       WHEN    4
         MOVE  NC"木"       TO  DSP-YOUBI (IX-GYO)
       WHEN    5
         MOVE  NC"金"       TO  DSP-YOUBI (IX-GYO)
     END-EVALUATE.
*  実行区分　
     MOVE  TB-NIT-F02 (IX-PAGE IX-GYO)  TO  DSP-JKKBN (IX-GYO).

     EVALUATE  DSP-JKKBN (IX-GYO)
       WHEN  "1"   MOVE  NC"実行　"   TO  DSP-JKKBNN (IX-GYO)
       WHEN  "9"   MOVE  NC"実行無"   TO  DSP-JKKBNN (IX-GYO)
       WHEN  OTHER MOVE  SPACE        TO  DSP-JKKBNN (IX-GYO)
     END-EVALUATE.

*  月次区分
     MOVE  TB-NIT-F10 (IX-PAGE IX-GYO) TO  DSP-GJKBN (IX-GYO).

     EVALUATE  DSP-GJKBN (IX-GYO)
       WHEN  "1"   MOVE  NC"実行　"   TO  DSP-GJKBNN(IX-GYO)
       WHEN  "9"   MOVE  NC"実行無"   TO  DSP-GJKBNN(IX-GYO)
       WHEN  OTHER MOVE  SPACE        TO  DSP-GJKBNN(IX-GYO)
     END-EVALUATE.

*  実行時間
     MOVE  TB-NIT-F03 (IX-PAGE IX-GYO) TO  DSP-JKTIME (IX-GYO).
*  発注日
     MOVE  TB-NIT-F05 (IX-PAGE IX-GYO) TO  DSP-HACDAY (IX-GYO).
*  納品日
     MOVE  TB-NIT-F06 (IX-PAGE IX-GYO) TO  DSP-NOUDAY (IX-GYO).
*  出荷日
     MOVE  TB-NIT-F07 (IX-PAGE IX-GYO) TO  DSP-SYUDAY (IX-GYO).
*  実行結果
     MOVE  TB-NIT-F08 (IX-PAGE IX-GYO) TO  DSP-KEKKA1 (IX-GYO).

     EVALUATE  TB-NIT-F08 (IX-PAGE IX-GYO)
       WHEN   "1"
         MOVE  NC"正常終了" TO  DSP-KEKKA2 (IX-GYO)
       WHEN   "9"
         MOVE  NC"異常終了" TO  DSP-KEKKA2 (IX-GYO)
       WHEN   OTHER
         MOVE  NC"未実行　" TO  DSP-KEKKA2 (IX-GYO)
     END-EVALUATE.

 GAMEN-EDTB-EXIT.
     EXIT.
****************************************************************
*           　明細  入力( PSW = 2 )
****************************************************************
 DSP-MEIS-SEC          SECTION.
     MOVE  "DSP-MEIS-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
     *>実行
       WHEN  "E000"
         PERFORM  MEIS-CHK-SEC
         IF  ERR-FLG = ZERO
             MOVE  18       TO  ERR-FLG
             MOVE  "3"      TO  PSW
         END-IF
     *>取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW
     *>終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
* 2011/10/12,S  S.I/NAV
     *>前頁
       WHEN  "F011"
         PERFORM  MEIS-CHK-SEC
         IF  ERR-FLG = ZERO
             PERFORM  DSP-BACKWDPAGE-SEC
         END-IF
     *>次頁
       WHEN  "F012"
         PERFORM  MEIS-CHK-SEC
         IF  ERR-FLG = ZERO
             PERFORM  DSP-FORWDPAGE-SEC
         END-IF
* 2011/10/12,E  S.I/NAV
       WHEN  OTHER
         MOVE  2            TO  ERR-FLG

     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             画面表示処理
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE  "DSP-WRITE-SEC"  TO  S-NAME.
* エラーメッセージセット
     IF  ERR-FLG   =    ZERO
         MOVE  SPACE               TO  DSP-ERRMSG
     ELSE
         MOVE  ERR-MSG-R (ERR-FLG) TO  DSP-ERRMSG
     END-IF.
* ガイドメッセージの設定
     IF  PSW = 2
         MOVE  PF-MSG1      TO  DSP-PFMSG
     END-IF.
     IF  PSW = 3
         MOVE  PF-MSG2      TO  DSP-PFMSG
     END-IF.
* 画面の表示
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FJH01031"       TO  DSP-FMT.
     MOVE  SPACE            TO  DSP-PRO.

     WRITE  DSP-FJH01031.

 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  "DSP-READ-SEC"   TO  S-NAME.

     MOVE  "NE"             TO  DSP-PRO. *> 非消去入力指定

     EVALUATE  PSW
    *> バッチＮｏ
       WHEN  "2"
         MOVE  "BODY"       TO  DSP-GRP
    *> 確認
       WHEN  "3"
         MOVE  "KAKNIN"     TO  DSP-GRP
     END-EVALUATE.

     MOVE  "FJH01031"       TO  DSP-FMT.
     READ  DSPFILE.
* 入力項目の属性を通常にする
 DSP-READ-010.
     MOVE  SPACE            TO  DSP-PRO.
     MOVE  ZERO             TO  ERR-FLG.

 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             　明細チェック
****************************************************************
 MEIS-CHK-SEC             SECTION.
     MOVE  "MEIS-CHK-SEC"   TO  S-NAME.

     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > 15
                   OR DSP-DATE(IX-GYO) = ZERO
       PERFORM KOMOKU-CHK-SEC

     END-PERFORM.

     IF  ERR-FLG NOT = ZERO
         GO TO  MEIS-CHK-EXIT
     END-IF.

* 画面のテーブルへの更新
     PERFORM  GAMEN-TO-TBL-SEC.

 MEIS-CHK-EXIT.
     EXIT.
****************************************************************
*    項目チェック
****************************************************************
 KOMOKU-CHK-SEC        SECTION.

     MOVE  "M"    TO  EDIT-OPTION OF DSP-JKKBN  (IX-GYO).
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-JKKBN  (IX-GYO).
     MOVE  "M"    TO  EDIT-OPTION OF DSP-GJKBN  (IX-GYO).
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-GJKBN  (IX-GYO).
     MOVE  "M"    TO  EDIT-OPTION OF DSP-JKTIME (IX-GYO).
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-JKTIME (IX-GYO).
     MOVE  "M"    TO  EDIT-OPTION OF DSP-HACDAY (IX-GYO).
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-HACDAY (IX-GYO).
     MOVE  "M"    TO  EDIT-OPTION OF DSP-NOUDAY (IX-GYO).
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-NOUDAY (IX-GYO).
     MOVE  "M"    TO  EDIT-OPTION OF DSP-SYUDAY (IX-GYO).
     MOVE  SPACE  TO  EDIT-CURSOR OF DSP-SYUDAY (IX-GYO).
*2017/02/07 NAV ST
* 日付チェック
*   システム日付－２０日算出
      MOVE  "6"               TO  LINK-IN-KBN.
      MOVE  WK-NOUDT-KIJYUN   TO  LINK-IN-YMD6.
      MOVE  DSP-DATE(IX-GYO)  TO  LINK-IN-YMD8.
      MOVE  ZERO              TO  LINK-OUT-RET.
      MOVE  ZERO              TO  LINK-OUT-YMD.
      CALL  "SKYDTCKB"      USING LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
      MOVE  LINK-OUT-YMD      TO  WK-NOUDT-KIJYUN-BI.
*2017/02/07 NAV ED

* 実行区分未入力チェック
     IF  DSP-JKKBN (IX-GYO) =  SPACE
         *> 2010/04/27 実行区分を空白にしたら明細全て初期化
         *> 実行区分を空白にしたら、明細項目を全て初期化する。
         MOVE  SPACE        TO  DSP-JKKBN  (IX-GYO)
         MOVE  SPACE        TO  DSP-GJKBN  (IX-GYO)
         MOVE  SPACE        TO  DSP-GJKBNN (IX-GYO)
         MOVE  ZERO         TO  DSP-JKTIME (IX-GYO)
         MOVE  SPACE        TO  DSP-JKKBNN (IX-GYO)
         MOVE  ZERO         TO  DSP-HACDAY (IX-GYO)
         MOVE  ZERO         TO  DSP-NOUDAY (IX-GYO)
         MOVE  ZERO         TO  DSP-SYUDAY (IX-GYO)
         *> **********************************************
         GO TO  KOMOKU-CHK-EXIT
     ELSE
 *>  実行区分入力値チェック
        IF DSP-JKKBN (IX-GYO) NOT = "1" AND "9"
           IF  ERR-FLG = ZERO
               MOVE  11     TO  ERR-FLG
           END-IF
           MOVE  "R"  TO  EDIT-OPTION OF DSP-JKKBN (IX-GYO)
           MOVE  "C"  TO  EDIT-CURSOR OF DSP-JKKBN (IX-GYO)
        ELSE
           IF  DSP-JKKBN (IX-GYO) = "1"
               MOVE  NC"実行　"  TO  DSP-JKKBNN (IX-GYO)
           END-IF
           IF  DSP-JKKBN (IX-GYO) = "9"
               MOVE  NC"実行無"  TO  DSP-JKKBNN(IX-GYO)
               GO TO  KOMOKU-CHK-EXIT
           END-IF
        END-IF
     END-IF.

* 月次区分入力値チェック
     IF  DSP-GJKBN (IX-GYO) NOT = "1" AND "9" AND SPACE
         IF  ERR-FLG = ZERO
             MOVE  12       TO   ERR-FLG
         END-IF
         MOVE  "R"   TO  EDIT-OPTION OF DSP-GJKBN (IX-GYO)
         MOVE  "C"   TO  EDIT-CURSOR OF DSP-GJKBN (IX-GYO)
     ELSE
         IF  DSP-GJKBN (IX-GYO) = "1"
             MOVE  NC"実行　"    TO  DSP-GJKBNN (IX-GYO)
         END-IF
         IF  DSP-GJKBN(IX-GYO) = "9"
             MOVE  NC"実行無"    TO  DSP-GJKBNN (IX-GYO)
         END-IF
     END-IF.

* 実行時間未入力・論理チェック
     IF      DSP-JKTIME (IX-GYO) = ZERO
         AND DSP-JKKBN  (IX-GYO) = 1
         IF  ERR-FLG = ZERO
             MOVE  3        TO  ERR-FLG
         END-IF
         MOVE  "R"   TO  EDIT-OPTION OF DSP-JKTIME (IX-GYO)
         MOVE  "C"   TO  EDIT-CURSOR OF DSP-JKTIME (IX-GYO)
     ELSE
  *> データ退避
         MOVE  DSP-JKTIME (IX-GYO)  TO  WK-JKTIME
          IF    WK-JKTIME-HH < ZERO
             OR WK-JKTIME-HH > 23
             IF  ERR-FLG = ZERO
                 MOVE  8         TO  ERR-FLG
             END-IF
             MOVE  "R"  TO  EDIT-OPTION OF DSP-JKTIME (IX-GYO)
             MOVE  "C"  TO  EDIT-CURSOR OF DSP-JKTIME (IX-GYO)
         END-IF

         IF  WK-JKTIME-MM NOT = ZERO AND 15
                            AND   30 AND 45
             IF  ERR-FLG =  ZERO
                 MOVE  8    TO  ERR-FLG
             END-IF
             MOVE  "R"  TO  EDIT-OPTION OF DSP-JKTIME (IX-GYO)
             MOVE  "C"  TO  EDIT-CURSOR OF DSP-JKTIME (IX-GYO)
         END-IF
     END-IF.

* 発注日論理エラーチェック
     IF  DSP-HACDAY (IX-GYO) NOT = ZERO
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  ZERO         TO  LINK-IN-YMD6
         MOVE  DSP-HACDAY (IX-GYO)  TO  LINK-IN-YMD8
         MOVE  ZERO         TO  LINK-OUT-RET
         MOVE  ZERO         TO  LINK-OUT-YMD
         CALL  "SKYDTCKB"  USING LINK-IN-KBN
                                 LINK-IN-YMD6
                                 LINK-IN-YMD8
                                 LINK-OUT-RET
                                 LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  5    TO  ERR-FLG
             END-IF
             MOVE  "R"   TO  EDIT-OPTION OF DSP-HACDAY (IX-GYO)
             MOVE  "C"   TO  EDIT-CURSOR OF DSP-HACDAY (IX-GYO)
         END-IF
     END-IF.

* 納品日未入力・論理エラーチェック
     IF      DSP-NOUDAY(IX-GYO) NOT NUMERIC
         OR  DSP-NOUDAY(IX-GYO) = ZERO
         IF  ERR-FLG =  ZERO
             MOVE   4       TO   ERR-FLG
         END-IF
         MOVE  "R"   TO  EDIT-OPTION OF DSP-NOUDAY (IX-GYO)
         MOVE  "C"   TO  EDIT-CURSOR OF DSP-NOUDAY (IX-GYO)
     ELSE
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  ZERO         TO  LINK-IN-YMD6
         MOVE  DSP-NOUDAY(IX-GYO)  TO  LINK-IN-YMD8
         MOVE  ZERO         TO  LINK-OUT-RET
         MOVE  ZERO         TO  LINK-OUT-YMD
         CALL  "SKYDTCKB"  USING LINK-IN-KBN
                                 LINK-IN-YMD6
                                 LINK-IN-YMD8
                                 LINK-OUT-RET
                                 LINK-OUT-YMD
         IF   LINK-OUT-RET  NOT = ZERO
              IF  ERR-FLG =  ZERO
                  MOVE  6   TO  ERR-FLG
              END-IF
              MOVE  "R"  TO  EDIT-OPTION OF DSP-NOUDAY (IX-GYO)
              MOVE  "C"  TO  EDIT-CURSOR OF DSP-NOUDAY (IX-GYO)
         ELSE
* 納品日日付チェック
              IF  DSP-NOUDAY (IX-GYO) > DSP-DATE (IX-GYO)
                  IF  ERR-FLG = ZERO
                      MOVE  13      TO  ERR-FLG
                  END-IF
                  MOVE "R" TO  EDIT-OPTION OF DSP-NOUDAY (IX-GYO)
                  MOVE "C" TO  EDIT-CURSOR OF DSP-NOUDAY (IX-GYO)
              END-IF
* 納品日基準日チェック
              IF  DSP-NOUDAY (IX-GYO) < WK-NOUDT-KIJYUN-BI
                  IF  ERR-FLG = ZERO
                      MOVE  19      TO  ERR-FLG
                  END-IF
                  MOVE "R" TO  EDIT-OPTION OF DSP-NOUDAY (IX-GYO)
                  MOVE "C" TO  EDIT-CURSOR OF DSP-NOUDAY (IX-GYO)
              END-IF
         END-IF
      END-IF.


* 出荷日論理エラーチェック
      IF  DSP-SYUDAY (IX-GYO) NOT = ZERO
          MOVE  "2"         TO  LINK-IN-KBN
          MOVE  ZERO        TO  LINK-IN-YMD6
          MOVE  DSP-SYUDAY (IX-GYO)  TO  LINK-IN-YMD8
          MOVE  ZERO        TO  LINK-OUT-RET
          MOVE  ZERO        TO  LINK-OUT-YMD
          CALL  "SKYDTCKB"  USING LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD
          IF  LINK-OUT-RET NOT = ZERO
              IF  ERR-FLG =  ZERO
                  MOVE  7   TO  ERR-FLG
              END-IF
              MOVE  "R"   TO  EDIT-OPTION OF DSP-SYUDAY (IX-GYO)
              MOVE  "C"   TO  EDIT-CURSOR OF DSP-SYUDAY (IX-GYO)
          END-IF
     *> 出荷日指定エラーチェック
          IF  DSP-SYUDAY (IX-GYO) > DSP-NOUDAY (IX-GYO)
              IF  ERR-FLG = ZERO
                  MOVE  9   TO  ERR-FLG
              END-IF
              MOVE  "R"   TO  EDIT-OPTION OF DSP-SYUDAY (IX-GYO)
              MOVE  "C"   TO  EDIT-CURSOR OF DSP-SYUDAY (IX-GYO)
          END-IF
* 出荷日基準日チェック
          IF  DSP-SYUDAY (IX-GYO) < WK-NOUDT-KIJYUN-BI
              IF  ERR-FLG = ZERO
                  MOVE  20      TO  ERR-FLG
              END-IF
              MOVE "R" TO  EDIT-OPTION OF DSP-SYUDAY (IX-GYO)
              MOVE "C" TO  EDIT-CURSOR OF DSP-SYUDAY (IX-GYO)
          END-IF
      END-IF.

 KOMOKU-CHK-EXIT.
     EXIT.
****************************************************************
*  画面内容テーブル格納処理       2011/10/12 追加
****************************************************************
 GAMEN-TO-TBL-SEC             SECTION.
     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > 15
       PERFORM  GAMEN-TO-TBLB-SEC

     END-PERFORM.
 GAMEN-TO-TBL-EXIT.
     EXIT.
****************************************************************
*  画面内容テーブル格納Ｂ処理     2011/10/12 追加
****************************************************************
 GAMEN-TO-TBLB-SEC            SECTION.
     IF ((IX-PAGE - 1) * 15) + IX-GYO  <=  IX-TBLMAX
   *> 明細行編集
        CONTINUE
     ELSE
   *> 空白行編集
        GO TO  GAMEN-TO-TBLB-EXIT
     END-IF.

*  実行区分
     MOVE  DSP-JKKBN (IX-GYO)   TO  TB-NIT-F02(IX-PAGE IX-GYO).
*  月次区分
     MOVE  DSP-GJKBN (IX-GYO)   TO  TB-NIT-F10(IX-PAGE IX-GYO).
*  実行時間
     MOVE  DSP-JKTIME(IX-GYO)   TO  TB-NIT-F03(IX-PAGE IX-GYO).
*  発注日
     MOVE  DSP-HACDAY(IX-GYO)   TO  TB-NIT-F05(IX-PAGE IX-GYO).
*  納品日
     MOVE  DSP-NOUDAY(IX-GYO)   TO  TB-NIT-F06(IX-PAGE IX-GYO).
*  出荷日
     MOVE  DSP-SYUDAY(IX-GYO)   TO  TB-NIT-F07(IX-PAGE IX-GYO).

 GAMEN-TO-TBLB-EXIT.
     EXIT.
***************************************************************
*  前頁表示処理       2011/10/12 追加
****************************************************************
 DSP-BACKWDPAGE-SEC             SECTION.
*    当ページのチェック
     PERFORM  MEIS-CHK-SEC
     IF  ERR-FLG NOT = ZERO
         GO TO  DSP-BACKWDPAGE-EXIT
     END-IF

*    前ページテーブルから明細項目セット　　　　
     IF IX-PAGE = 1
        IF  ERR-FLG = ZERO
            MOVE  14        TO  ERR-FLG
        END-IF
        GO TO  DSP-BACKWDPAGE-EXIT
     END-IF.

     COMPUTE  IX-PAGE = IX-PAGE - 1.

     PERFORM  GAMEN-EDT-SEC.

 DSP-BACKWDPAGE-EXIT.
     EXIT.
****************************************************************
*  次頁表示処理       2011/10/12 追加
****************************************************************
 DSP-FORWDPAGE-SEC             SECTION.
*    当ページのチェック
     PERFORM  MEIS-CHK-SEC
     IF  ERR-FLG NOT = ZERO
         GO TO  DSP-FORWDPAGE-EXIT
     END-IF

*    次ページテーブルから明細項目セット
     IF IX-PAGE >= IX-PAGEMAX
        IF  ERR-FLG = ZERO
            MOVE  15        TO  ERR-FLG
        END-IF
        GO TO  DSP-FORWDPAGE-EXIT
     END-IF.

     COMPUTE  IX-PAGE = IX-PAGE + 1.

     PERFORM  GAMEN-EDT-SEC.

 DSP-FORWDPAGE-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 3 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  "DSP-KAKU-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
     *> 実行
       WHEN  "E000"
         PERFORM  NIT-WRITE-SUB
         MOVE  "1"          TO  PSW

     *> 取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW

     *> 終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG

     *> 項目戻し
       WHEN  "F006"
         MOVE  "2"          TO  PSW

       WHEN  OTHER
         MOVE  2            TO  ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             日時更新条件マスタ更新　　　　
****************************************************************
 NIT-WRITE-SUB    SECTION.
     MOVE     "NIT-WRITE-SUB"  TO   S-NAME.

* 2011/10/12,S  S.I/NAV
     PERFORM  VARYING IX-TBL FROM 1 BY 1
                UNTIL IX-TBL > IX-TBLMAX
       PERFORM  NIT-WRITE-SUBB

     END-PERFORM.
* 2011/10/12,E  S.I/NAV

 NIT-WRITE-EXIT.
     EXIT.
****************************************************************
*  日時更新条件マスタ更新Ｂ       2011/10/12 追加
****************************************************************
 NIT-WRITE-SUBB    SECTION.
     MOVE  "NIT-WRITE-SUBB-SEC"  TO  S-NAME.

     MOVE  SPACE            TO  NIT-REC.
     INITIALIZE  NIT-REC.

     MOVE  TB-NIT-F01R (IX-TBL)  TO  NIT-F01.

     READ JHMNITF
       INVALID  KEY
         DISPLAY "***INVALID ERR*** " DSP-DATE(IX-GYO) UPON CONS
         STOP RUN
         GO TO  NIT-WRITE-SUBB-EXIT
     END-READ.

     MOVE  TB-NIT-F02R (IX-TBL) TO  NIT-F02. *>実行区分
     MOVE  TB-NIT-F10R (IX-TBL) TO  NIT-F10. *>月次区分
     MOVE  TB-NIT-F03R (IX-TBL) TO  NIT-F03. *>実行時間
     MOVE  TB-NIT-F05R (IX-TBL) TO  NIT-F05. *>日次更新・発注日
     MOVE  TB-NIT-F06R (IX-TBL) TO  NIT-F06. *>日次更新・納入日
     MOVE  TB-NIT-F07R (IX-TBL) TO  NIT-F07. *>日次更新・出荷日
     REWRITE  NIT-REC.

 NIT-WRITE-SUBB-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.
* ファイル ＣＬＯＳＥ
* 2011/10/12,S  S.I/NAV
     CLOSE  HJYOKEN.
* 2011/10/12,E  S.I/NAV
     CLOSE  DSPFILE  JHMNITF.

 END-EXIT.
     EXIT.
*******************< PROGRAM-END SJH0015I >*********************

```
