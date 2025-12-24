# SSY3882B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY3882B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　作場変更、出荷数変更　　　        *
*    モジュール名　　　　：　ナフコ作場数量一括変更ＤＴ更新指示*
*    作成日／作成者　　　：　2018/12/03 NAV TAKAHASHI          *
*    処理概要　　　　　　：　パラメタを受け取り、ナフコ作場数　*
*                          　量一括変更ＤＴを読み、サブにて更　*
*                          　新する（ナフコ基本情報、売上伝票）*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/12/03 高橋_　　新規作成
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3882B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2018/12/03.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*作場数量一括変更データ
     SELECT  SKSRHEF   ASSIGN    TO        SKSRHEL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       HEN-F01
                                           HEN-F02
                                           HEN-F03
                                           HEN-F04
                                           HEN-F05
                                           HEN-F09
                                           HEN-F10
                                           HEN-F11
                                           HEN-F12
                                           HEN-F13
                       FILE      STATUS    HEN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 作場数量一括変更データ
****************************************************************
 FD  SKSRHEF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SKSRHEF   OF   XFDLIB
                       JOINING   HEN       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  HEN-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  READ-CNT                 PIC  9(07)  VALUE  ZERO.
     03  WT-CNT                   PIC  9(07)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
     03  WK-TIME6                 PIC  9(06)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08).
*
 01  FILE-ERR.
     03  HEN-ERR           PIC N(20) VALUE
                        NC"作場数量一括変更データエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 01  LINK-SUB-IN.
     03  LINK-SUB-BUMON    PIC X(04).
     03  LINK-SUB-TANCD    PIC X(02).
     03  LINK-SUB-KANRI    PIC 9(08).
     03  LINK-SUB-BTDATE   PIC 9(08).
     03  LINK-SUB-BTTIME   PIC 9(04).
     03  LINK-SUB-BTTOKC   PIC 9(08).
     03  LINK-SUB-SAKUBA   PIC X(02).
     03  LINK-SUB-NOUDT    PIC 9(08).
     03  LINK-SUB-TENCD    PIC 9(05).
     03  LINK-SUB-NFSHOCD  PIC X(08).
     03  LINK-SUB-DENNO    PIC 9(09).
     03  LINK-SUB-GYO      PIC 9(02).
     03  LINK-SUB-SAKKBN   PIC X(01).
     03  LINK-SUB-SURKBN   PIC X(01).
     03  LINK-SUB-HENSAKB  PIC X(02).
     03  LINK-SUB-HENSURY  PIC 9(05).
     03  LINK-SUB-KIHDATE  PIC 9(08).
     03  LINK-SUB-KIHTIME  PIC 9(06).
 01  LINK-SUB-OUT.
     03  LINK-SUB-KOUSIN1  PIC X(01).
     03  LINK-SUB-KOUSIN2  PIC X(01).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-BUMON          PIC  X(04).
 01  LINK-TANCD          PIC  X(02).
 01  LINK-UPD-DATE       PIC  9(08).
 01  LINK-UPD-TIME       PIC  9(06).
 01  LINK-KOUSIN-KENSU   PIC  9(07).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-BUMON
                                     LINK-TANCD
                                     LINK-UPD-DATE
                                     LINK-UPD-TIME
                                     LINK-KOUSIN-KENSU.
**************************************************************
 DECLARATIVES.
 HEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SKSRHEF.
     DISPLAY     HEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HEN-ST    UPON      CONS.
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
     STOP  RUN.
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
     MOVE      WK-TIME(1:6)       TO   WK-TIME6.
*
*****DISPLAY "LINK-UPD-DATE = " LINK-UPD-DATE  UPON CONS.
*****DISPLAY "LINK-UPD-TIME = " LINK-UPD-TIME  UPON CONS.
*ファイルのＯＰＥＮ
     OPEN     I-O       SKSRHEF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
     MOVE     ZERO                TO   LINK-KOUSIN-KENSU.
*ナフコ基本情報ファイルスタート
     MOVE     SPACE               TO   HEN-REC.
     INITIALIZE                        HEN-REC.
     MOVE     LINK-UPD-DATE       TO   HEN-F01.
     MOVE     LINK-UPD-TIME       TO   HEN-F02.
     START  SKSRHEF  KEY  >=   HEN-F01  HEN-F02  HEN-F03
                               HEN-F04  HEN-F05  HEN-F09
                               HEN-F10  HEN-F11  HEN-F12
                               HEN-F13
            INVALID
            MOVE  "END"           TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                    TO   INIT-EXIT
     END-START.
*ナフコ基本情報ファイル読込
     PERFORM  SKSRHEF-READ-SEC.
*
     IF   END-FLG =  "END"
          DISPLAY NC"＃対象データ無し２＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    ナフコ基本情報ファイル読込
****************************************************************
 SKSRHEF-READ-SEC      SECTION.
     MOVE "SKSRHEF-READ-SEC" TO   S-NAME.
*
     READ  SKSRHEF  NEXT  AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   NFJOHOF-READ-EXIT
           NOT  AT  END
           ADD     1         TO   READ-CNT
     END-READ.
*
     IF    READ-CNT(5:3)  =  "000" OR "500"
           DISPLAY "#READ-CNT = " READ-CNT UPON CONS
     END-IF.
*バッチ番号のチェック
     IF    LINK-UPD-DATE =  HEN-F01
     AND   LINK-UPD-TIME =  HEN-F02
           CONTINUE
     ELSE
           MOVE   "END"      TO   END-FLG
           GO                TO   NFJOHOF-READ-EXIT
     END-IF.
*
 NFJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*パラメタ初期化
     INITIALIZE                        LINK-SUB-IN.
     INITIALIZE                        LINK-SUB-OUT.
*パラメタセット
*　部門
     MOVE      LINK-BUMON         TO   LINK-SUB-BUMON.
*　担当者ＣＤ
     MOVE      LINK-TANCD         TO   LINK-SUB-TANCD.
*　管理番号
     MOVE      HEN-F05            TO   LINK-SUB-KANRI.
*　バッチ日付
     MOVE      HEN-F06            TO   LINK-SUB-BTDATE.
*　バッチ時刻
     MOVE      HEN-F07            TO   LINK-SUB-BTTIME.
*　バッチ取引先
     MOVE      HEN-F08            TO   LINK-SUB-BTTOKC.
*　作場ＣＤ
     MOVE      HEN-F09            TO   LINK-SUB-SAKUBA.
*　納品日
     MOVE      HEN-F11            TO   LINK-SUB-NOUDT.
*　店舗ＣＤ
     MOVE      HEN-F10            TO   LINK-SUB-TENCD.
*　ナフコ商品ＣＤ
     MOVE      HEN-F14            TO   LINK-SUB-NFSHOCD.
*　伝票番号
     MOVE      HEN-F12            TO   LINK-SUB-DENNO.
*　行番号
     MOVE      HEN-F13            TO   LINK-SUB-GYO.
*　作場変更区分
     MOVE      HEN-F19            TO   LINK-SUB-SAKKBN.
*　数量変更区分
     MOVE      HEN-F20            TO   LINK-SUB-SURKBN.
*　変更作場
     MOVE      HEN-F22            TO   LINK-SUB-HENSAKB.
*　数量変更
     MOVE      HEN-F21            TO   LINK-SUB-HENSURY.
*　基本日付
     MOVE      HEN-F01            TO   LINK-SUB-KIHDATE.
*　基本時刻
     MOVE      HEN-F02            TO   LINK-SUB-KIHTIME.
*****DISPLAY "LINK-SUB-IN  = " LINK-SUB-IN   UPON CONS.
*　サブルーチンコール
     CALL     "SSY3883B"       USING   LINK-SUB-IN
                                       LINK-SUB-OUT.
     ADD       1                  TO   WT-CNT.
*****DISPLAY "LINK-SUB-OUT = " LINK-SUB-OUT  UPON CONS.
*  更新確認
     IF    LINK-SUB-KOUSIN1  = "1"
     AND   LINK-SUB-KOUSIN2  = "1"
           MOVE    "1"            TO   HEN-F99
           REWRITE  HEN-REC
     END-IF.
*
*ナフコ基本情報ファイル読込
     PERFORM   SKSRHEF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             SKSRHEF.
*
     DISPLAY NC"＃更新起動件数＝" WT-CNT  UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  SSY3882B   END PROGRAM  >>******************

```
