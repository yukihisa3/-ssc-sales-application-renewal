****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＲＰＡ導入　　　　　　　　　　　　*
*    業務名　　　　　　　：　欠品出力　　　　　　　　　        *
*    モジュール名　　　　：　オンライン欠品データ出力　　　　　*
*    作成日／更新日　　　：　20/03/18                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　パラメタを受け取り、対象の欠品デ　*
*                            ータを抽出する。　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY0806B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         20/03/18.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*欠品ファイル
     SELECT  SHTDENLA   ASSIGN   TO        SHTDENLA
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       DEN-F46
                                           DEN-F47
                                           DEN-F01
                                           DEN-F48
                                           DEN-F02
                                           DEN-F04
                                           DEN-F051
                                           DEN-F07
                                           DEN-F112
                                           DEN-F03
                       FILE      STATUS    DEN-ST.
*ＤＣＭ基本情報データ
     SELECT  DJJOHOL1  ASSIGN    TO        DJJOHOL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JOH-K01
                                           JOH-K02
                                           JOH-K03
                                           JOH-K04
                                           JOH-K05
                                           JOH-K06
                                           JOH-K07
                                           JOH-K08
                       FILE      STATUS    JOH-ST.
*
*オンライン欠品データ
     SELECT  ONLKEPF   ASSIGN    TO        ONLKEPL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KEP-F01
                                           KEP-F02
                                           KEP-F03
                                           KEP-F04
                                           KEP-F05
                                           KEP-F08
                                           KEP-F06
                                           KEP-F07
                       FILE      STATUS    KEP-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 欠品ファイル                     *
****************************************************************
 FD  SHTDENLA
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SHTDENLA   OF   XFDLIB
                       JOINING   DEN       AS   PREFIX.
****************************************************************
*  FILE= ＤＣＭ基本情報データ                                *
****************************************************************
 FD  DJJOHOL1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      DJJOHOF   OF   XFDLIB
                       JOINING   JOH       AS   PREFIX.
****************************************************************
*    FILE = オンライン欠品データ                               *
****************************************************************
 FD  ONLKEPF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ONLKEPF   OF   XFDLIB
                       JOINING   KEP       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  JOH-ST                   PIC  X(02).
     03  KEP-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
 01  READ-CNT                     PIC  9(08)  VALUE  ZERO.
 01  REWT-CNT                     PIC  9(08)  VALUE  ZERO.
 01  WRTD-CNT                     PIC  9(08)  VALUE  ZERO.
 01  KEPD-CNT                     PIC  9(08)  VALUE  ZERO.
 01  ONLKEPF-INV-FLG              PIC  X(03)  VALUE  SPACE.
 01  DJJOHOF-INV-FLG              PIC  X(03)  VALUE  SPACE.
 01  WK-NOUDT                     PIC  9(08)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
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
*
 01  FILE-ERR.
     03  DEN-ERR           PIC N(20) VALUE
                        NC"売上伝票Ｆエラー".
     03  JOH-ERR           PIC N(20) VALUE
                        NC"ＤＣＭ基本情報エラー".
     03  KEP-ERR           PIC N(20) VALUE
                        NC"オンライン欠品ＤＴエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                   SECTION.
 01  PARA-HIDUKE           PIC 9(08).
 01  PARA-JIKAN            PIC 9(04).
 01  PARA-TOKCD            PIC 9(08).
 01  PARA-SOKCD            PIC X(02).
 01  PARA-NOUDT            PIC 9(08).
**************************************************************
 PROCEDURE             DIVISION    USING
                                   PARA-HIDUKE
                                   PARA-JIKAN
                                   PARA-TOKCD
                                   PARA-SOKCD
                                   PARA-NOUDT.
**************************************************************
 DECLARATIVES.
 KPN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENLA.
     DISPLAY     DEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JOH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DJJOHOL1.
     DISPLAY     JOH-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JOH-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KEP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ONLKEPF.
     DISPLAY     KEP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KEP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*                   MAIN       MODULE                3.0       *
****************************************************************
 PROCESS-SEC             SECTION.
     MOVE     "PROCESS-SEC" TO   S-NAME.
     DISPLAY  "***   SSY0806B   START    ***"  UPON CONS.
*
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC   UNTIL     END-FLG  =   "END".
     PERFORM   END-SEC.
     STOP      RUN.
*
 PROCESS-EXIT.
     EXIT.
****************************************************************
*             ＰＲＴファイル初期処理                 4.0       *
****************************************************************
 INIT-SEC                SECTION.
     MOVE     "INIT-SEC"    TO   S-NAME.
     DISPLAY "PARA-HIDUKE = " PARA-HIDUKE  UPON   CONS.
     DISPLAY "PARA-JIKAN  = " PARA-JIKAN   UPON   CONS.
     DISPLAY "PARA-TOKCD  = " PARA-TOKCD   UPON   CONS.
     DISPLAY "PARA-SOKCD  = " PARA-SOKCD   UPON   CONS.
     DISPLAY "PARA-NOUDT  = " PARA-NOUDT   UPON   CONS.
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
*ファイルのＯＰＥＮ
     OPEN     INPUT     SHTDENLA  DJJOHOL1.
     OPEN     I-O       ONLKEPF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*売上伝票ファイルスタート
     MOVE     SPACE               TO   DEN-REC.
     INITIALIZE                        DEN-REC.
     MOVE     PARA-HIDUKE         TO   DEN-F46.
     MOVE     PARA-JIKAN          TO   DEN-F47.
     MOVE     PARA-TOKCD          TO   DEN-F01.
     MOVE     PARA-SOKCD          TO   DEN-F48.
     DISPLAY "DEN-F46 = " DEN-F46  UPON CONS.
     DISPLAY "DEN-F47 = " DEN-F47  UPON CONS.
     DISPLAY "DEN-F01 = " DEN-F01  UPON CONS.
     DISPLAY "DEN-F48 = " DEN-F48  UPON CONS.
     START   SHTDENLA  KEY  IS  >=  DEN-F46  DEN-F47  DEN-F01
                                    DEN-F48  DEN-F02  DEN-F04
                                    DEN-F051 DEN-F07  DEN-F112
                                    DEN-F03
     INVALID  MOVE   "END"        TO   END-FLG
              DISPLAY NC"＃対象データ無！！　ＳＴ" UPON CONS
              GO                  TO   INIT-EXIT
     END-START.
*
     PERFORM  DEN-READ-SEC.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃対象データ無！！初ＲＤ" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             ＰＲＴファイルメイン処理               5.0       *
****************************************************************
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"    TO   S-NAME.
*オンライン欠品データ存在チェック
     MOVE     SPACE         TO   KEP-REC
     INITIALIZE                  KEP-REC.
*キーセット
     MOVE     DEN-F46       TO   KEP-F01.
     MOVE     DEN-F47       TO   KEP-F02.
     MOVE     DEN-F01       TO   KEP-F03.
     MOVE     DEN-F48       TO   KEP-F04.
     MOVE     DEN-F07       TO   KEP-F05.
     MOVE     DEN-F02       TO   KEP-F06.
     MOVE     DEN-F03       TO   KEP-F07.
     MOVE     DEN-F112      TO   KEP-F08.
*オンライン欠品データ読み込み
     READ     ONLKEPF
              INVALID
              MOVE "INV"    TO   ONLKEPF-INV-FLG
              NOT  INVALID
              MOVE SPACE    TO   ONLKEPF-INV-FLG
     END-READ.
*項目をセット
*****存在しない場合
     IF       ONLKEPF-INV-FLG  =  SPACE
              MOVE SPACE             TO   KEP-REC
              INITIALIZE                  KEP-REC
              MOVE     DEN-F46       TO   KEP-F01
              MOVE     DEN-F47       TO   KEP-F02
              MOVE     DEN-F01       TO   KEP-F03
              MOVE     DEN-F48       TO   KEP-F04
              MOVE     DEN-F07       TO   KEP-F05
              MOVE     DEN-F02       TO   KEP-F06
              MOVE     DEN-F03       TO   KEP-F07
              MOVE     DEN-F112      TO   KEP-F08
     END-IF.
*項目をセット
*****ＪＡＮＣＤ（相手商品ＣＤ）
     MOVE     DEN-F25       TO   KEP-F09.
*****サカタ商品ＣＤ
     MOVE     DEN-F1411     TO   KEP-F10.
*****サカタ品単ＣＤ
     MOVE     DEN-F1412     TO   KEP-F11.
*****商品名１
     MOVE     DEN-F1421     TO   KEP-F12.
*****商品名２
     MOVE     DEN-F1422     TO   KEP-F13.
*****発注数
     MOVE     DEN-F50       TO   KEP-F14.
*****出荷数
     MOVE     DEN-F15       TO   KEP-F15.
*****欠品数
     COMPUTE  KEP-F16  =  DEN-F50  -  DEN-F15.
*****ＤＣＭ伝票番号
     IF       DEN-F01  =  13938  OR  139381  OR  17137
                       OR 171371 OR  100403  OR  100404
                       OR 100427 OR  100428  OR  100441
                       OR 100442
              PERFORM  DJJOHOF-READ-SEC
              IF  DJJOHOF-INV-FLG  =  "INV"
                  MOVE DEN-F02   TO   KEP-F17
              ELSE
                  MOVE JOH-F03   TO   KEP-F17
              END-IF
     ELSE
              MOVE DEN-F02       TO   KEP-F17
     END-IF.
*レコード出力
     IF       ONLKEPF-INV-FLG  =  "INV"
              WRITE  KEP-REC
              ADD    1           TO   WRTD-CNT
     ELSE
              REWRITE KEP-REC
              ADD    1           TO   REWT-CNT
     END-IF.
*
     PERFORM  DEN-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             　売上伝票ファイル読込　　                     *
****************************************************************
 DEN-READ-SEC          SECTION.
     MOVE     "DEN-READ-SEC"      TO   S-NAME.
*
     READ     SHTDENLA       AT    END
              MOVE         "END"  TO   END-FLG
              GO   TO   DEN-READ-EXIT
         NOT  AT   END
              ADD        1  TO    READ-CNT
     END-READ.
*
     IF       READ-CNT(6:3)  =  "000"  OR "500"
              DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*バッチ番号のチェック
*****DISPLAY "DEN-F46 = " DEN-F46 UPON CONS.
*****DISPLAY "DEN-F47 = " DEN-F47 UPON CONS.
*****DISPLAY "DEN-F01 = " DEN-F01 UPON CONS.
     IF       PARA-HIDUKE  =  DEN-F46
     AND      PARA-JIKAN   =  DEN-F47
     AND      PARA-TOKCD   =  DEN-F01
              CONTINUE
     ELSE
              MOVE  "END"     TO    END-FLG
              GO              TO    DEN-READ-EXIT
     END-IF.
*取引先ＣＤチェック
*****IF       PARA-TOKCD   NOT =  ZERO
*             IF   PARA-TOKCD   =  DEN-F01
*                  CONTINUE
*             ELSE
*                  GO         TO    DEN-READ-SEC
*             END-IF
*****END-IF.
*倉庫ＣＤチェック
     IF       PARA-SOKCD  NOT =  SPACE
              IF   PARA-SOKCD  =  DEN-F48
                   CONTINUE
              ELSE
                   GO         TO    DEN-READ-SEC
              END-IF
     END-IF.
*納品日チェック
     MOVE     DEN-F112        TO    WK-NOUDT.
     IF       PARA-NOUDT  NOT =  ZERO
              IF   PARA-NOUDT  =  WK-NOUDT
                   CONTINUE
              ELSE
                   GO         TO    DEN-READ-SEC
              END-IF
     END-IF.
*欠品チェック
     IF       DEN-F15  =  DEN-F50
              GO              TO    DEN-READ-SEC
     END-IF.
*
     ADD      1               TO    KEPD-CNT.
*
 DEN-READ-EXIT.
     EXIT.
****************************************************************
*    ＤＣＭ基本情報ファイル読込　　　　
****************************************************************
 DJJOHOF-READ-SEC      SECTION.
*
     MOVE     "DJJOHOF-RAED-SEC"  TO   S-NAME.
*キーセット
     MOVE      DEN-F46        TO    JOH-K01.
     MOVE      DEN-F47        TO    JOH-K02.
     MOVE      DEN-F01        TO    JOH-K03.
     MOVE      DEN-F48        TO    JOH-K04.
     MOVE      DEN-F07        TO    JOH-K05.
     MOVE      DEN-F02        TO    JOH-K06.
     MOVE      DEN-F03        TO    JOH-K07.
     MOVE      DEN-F112       TO    JOH-K08.
*****DISPLAY "JOH-F01 = " JOH-K01  UPON  CONS.
*    DISPLAY "JOH-F02 = " JOH-K02  UPON  CONS.
*    DISPLAY "JOH-F03 = " JOH-K03  UPON  CONS.
*    DISPLAY "JOH-F04 = " JOH-K04  UPON  CONS.
*    DISPLAY "JOH-F05 = " JOH-K05  UPON  CONS.
*    DISPLAY "JOH-F06 = " JOH-K06  UPON  CONS.
*    DISPLAY "JOH-F07 = " JOH-K07  UPON  CONS.
*****DISPLAY "JOH-F08 = " JOH-K08  UPON  CONS.
*ＤＣＭ基本情報ファイル読込
     READ      DJJOHOL1
               INVALID
               MOVE  "INV"    TO    DJJOHOF-INV-FLG
               NOT  INVALID
               MOVE  SPACE    TO    DJJOHOF-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             SHTDENLA  DJJOHOL1  ONLKEPF.
*
     DISPLAY  "READ-CNT    = " READ-CNT         UPON CONS.
     DISPLAY  "KEPD-CNT    = " KEPD-CNT         UPON CONS.
     DISPLAY  "WRITE-CNT   = " WRTD-CNT         UPON CONS.
     DISPLAY  "REWRITE-CNT = " REWT-CNT         UPON CONS.
*
     DISPLAY  "***   SSY0806B   END      ***"  UPON CONS.
**
 END-EXIT.
     EXIT.
*****************<<  SSY0801L   END PROGRAM  >>******************
