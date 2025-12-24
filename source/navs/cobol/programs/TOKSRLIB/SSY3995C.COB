***********************************************************
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコ新ＥＤＩシステム　　         *
*    業務名　　　　　：ナフコデータ管理                   *
*    モジュール名　　：ナフコチェックデータ作成（受領）   *
*    作成日／作成者　：2020/05/25    NAV                  *
*    処理概要　　　　：                                   *
*      受信日を受け取り、ナフコ受領データより、　　　　　 *
*      ナフコチェックデータを作成する。（受信日Ｖｅｒ)    *
*    更新日／更新者　：                                   *
*    更新内容　　　　：　　　　　　　　　　　　　　　　　 *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3995C.
 AUTHOR.               ASS.
 DATE-WRITTEN.         2020/05/25.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
* ナフコ受領データ
     SELECT  NFJYURF
       ASSIGN         TO  NFJYURL3
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  SEQUENTIAL
       RECORD KEY     IS  JR3-F01   *> 受信日
                          JR3-F05   *> 店舗ＣＤ
                          JR3-F07   *> 伝票番号
                          JR3-F08   *> 行番号
       FILE STATUS    IS  JR3-ST.

* ナフコ商品マスタ
     SELECT  NFSHOMS
       ASSIGN         TO  NFSHOMS1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  SYO-F01   *> ナフコ商品ＣＤ
       FILE STATUS    IS  SYO-ST.

* ナフコチェックデータ
     SELECT  NFCKDTF
       ASSIGN         TO  NFCKDTL1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  CKD-F00   *> 取引先ＣＤ
                          CKD-F01   *> 店舗ＣＤ
                          CKD-F05   *> 店舗ＣＤ
                          CKD-F06   *> 店舗ＣＤ
                          CKD-F03   *> 伝票番号
                          CKD-F04   *> 行番号
                          CKD-F07   *> ナフコ商品ＣＤ
       FILE STATUS    IS  CKD-ST.

*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = ナフコ受領データ                                   *
****************************************************************
 FD  NFJYURF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFJYURF   OF   XFDLIB
                       JOINING   JR3       AS   PREFIX.
****************************************************************
*    FILE = ナフコ商品マスタ                                   *
****************************************************************
 FD  NFSHOMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFSHOMS   OF   XFDLIB
                       JOINING   SYO       AS   PREFIX.
****************************************************************
*    FILE = ナフコチェックデータ                               *
****************************************************************
 FD  NFCKDTF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFCKDTF   OF   XFDLIB
                       JOINING   CKD       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  ST1-ST             PIC  X(02).
     03  JR3-ST             PIC  X(02).
     03  SYO-ST             PIC  X(02).
     03  CKD-ST             PIC  X(02).
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)  VALUE " *** ".
     03  S-NAME             PIC  X(30).
***  エラーファイル
 01  FILE-ERR.
     03  ST1-ERR           PIC N(20) VALUE
         NC"数量訂正ファイル１・エラー".
     03  JR3-ERR           PIC  N(20)  VALUE
         NC"ナフコ受領ファイル３・エラー".
     03  SYO-ERR           PIC N(20) VALUE
         NC"ナフコ商品マスタ１・エラー".
     03  CKD-ERR           PIC N(20) VALUE
         NC"ナフコチェックデータ１・エラー".

*読込フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE SPACE.
     03  NFCKDTF-INV-FLG    PIC  X(03)  VALUE SPACE.
     03  NFSUTEF-INV-FLG    PIC  X(03)  VALUE SPACE.
     03  NFSHOMS-INV-FLG    PIC  X(03)  VALUE SPACE.
*読込・書込カウント領域
 01  CNT-AREA.
     03  NFJYURF-RD-CNT   PIC  9(07)  VALUE ZERO.
     03  SKIP-CNT           PIC  9(07)  VALUE ZERO.
     03  NFCKDTF-WT-CNT     PIC  9(07)  VALUE ZERO.
     03  NFCKDTF-RW-CNT     PIC  9(07)  VALUE ZERO.


 01  DATE-AREA.
     03  WK-DATE            PIC  9(06).
     03  SYS-DATE           PIC  9(08).
 01  WK-TIME.
     03  SYS-TIME           PIC  9(06).


*日付変換サブルーチン
 01  SKYDTCKB-AREA.
     03  SKYDTCKB-IN-KBN          PIC  X(01).
     03  SKYDTCKB-IN-YMD6         PIC  9(06).
     03  SKYDTCKB-IN-YMD8         PIC  9(08).
     03  SKYDTCKB-OUT-RET         PIC  X(01).
     03  SKYDTCKB-OUT-YMD         PIC  9(08).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
* 入力パラメータ
 01  PA-JYUSNST   PIC  9(08). *> 受信日
 01  PA-JYUSNED   PIC  9(08). *> 受信日
*
**************************************************************
 PROCEDURE             DIVISION
                               USING   PA-JYUSNST  PA-JYUSNED.
**************************************************************
 DECLARATIVES.
 JR3-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJYURF.
     DISPLAY     JR3-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JR3-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSHOMS.
     DISPLAY     SYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 CKD-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFCKDTF.
     DISPLAY     CKD-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     CKD-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               1.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.

* ファイルのＯＰＥＮ
     OPEN  INPUT  NFJYURF.
     OPEN  INPUT  NFSHOMS.
     OPEN  I-O    NFCKDTF.

* システム日付取得
     ACCEPT  WK-DATE  FROM DATE.
     ACCEPT  WK-TIME  FROM TIME.
     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  WK-DATE          TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                             SKYDTCKB-IN-YMD6
                             SKYDTCKB-IN-YMD8
                             SKYDTCKB-OUT-RET
                             SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD TO  SYS-DATE.

* 初期値設定

* 基本情報ファイルの初期読み込み
     MOVE  SPACE            TO  END-FLG.
     MOVE  SPACE            TO  JR3-REC.
     INITIALIZE                 JR3-REC.
     MOVE  PA-JYUSNST       TO  JR3-F01.  *> 受信日
     START NFJYURF KEY IS >= JR3-F01  JR3-F05  JR3-F07
                             JR3-F08
           INVALID
           DISPLAY NC"＃対象データ無し（ＳＴ）＃" UPON CONS
           MOVE    4000     TO  PROGRAM-STATUS
           MOVE    "END"    TO  END-FLG
           GO               TO  INIT-EXIT
     END-START.
* 基本情報ファイル読み込み
     PERFORM  NFJYURF-READ-SEC.
*
     IF   END-FLG  =  "END"
          DISPLAY NC"＃対象データ無し（初Ｒ）＃" UPON CONS
          MOVE    4000     TO  PROGRAM-STATUS
          MOVE    "END"    TO  END-FLG
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*  ナフコ受領ファイル読込み
****************************************************************
 NFJYURF-READ-SEC            SECTION.
     MOVE  "NFJYURF-READ-SEC"  TO  S-NAME.
*
     READ  NFJYURF
           NEXT  AT  END
           MOVE     "END"      TO  END-FLG
           GO                  TO  NFJYURF-READ-EXIT
     END-READ.
*
     ADD   1                   TO  NFJYURF-RD-CNT.
     IF  NFJYURF-RD-CNT(5:3) = "000" OR  "500"
         DISPLAY "## READ-CNT = " NFJYURF-RD-CNT " #"
                 UPON CONS
     END-IF.
*
     IF  PA-JYUSNST  <=  JR3-F01
     AND PA-JYUSNED  >=  JR3-F01
           CONTINUE
     ELSE
           MOVE     "END"      TO  END-FLG
           GO                  TO  NFJYURF-READ-EXIT
     END-IF.
*
 NFJYURF-READ-EXIT.
     EXIT.
****************************************************************
*  ナフコチェックデータ存在チェック
****************************************************************
 NFCKDTF-READ-SEC            SECTION.
     MOVE  "NFCKDTF-READ-SEC" TO S-NAME.
*
     MOVE   284202            TO CKD-F00.
     MOVE   JR3-F05           TO CKD-F01.
     MOVE   JR3-F03           TO CKD-F05.
     MOVE   JR3-F04           TO CKD-F06.
     MOVE   JR3-F07           TO CKD-F03.
     MOVE   JR3-F08           TO CKD-F04.
     MOVE   JR3-F10           TO CKD-F07.
*
     READ  NFCKDTF
           INVALID     MOVE  "INV"    TO  NFCKDTF-INV-FLG
           NOT INVALID MOVE  SPACE    TO  NFCKDTF-INV-FLG
     END-READ.
*
 NFCKDTF-READ-EXIT.
     EXIT.
****************************************************************
*  メイン処理                                        2.0       *
****************************************************************
 MAIN-SEC               SECTION.
     MOVE  "MAIN-SEC"   TO  S-NAME.
*
     PERFORM  NFCKDTF-READ-SEC.
*
     IF  NFCKDTF-INV-FLG  =  "INV"
         MOVE   SPACE        TO   CKD-REC
         INITIALIZE               CKD-REC
     END-IF.
*
     MOVE     284202         TO   CKD-F00.
     MOVE     JR3-F05        TO   CKD-F01.
     MOVE     SPACE          TO   CKD-F02.
     MOVE     JR3-F07        TO   CKD-F03.
     MOVE     JR3-F08        TO   CKD-F04.
     MOVE     JR3-F03        TO   CKD-F05.
     MOVE     JR3-F04        TO   CKD-F06.
     MOVE     JR3-F10        TO   CKD-F07.
*商品名取得
*ナフコ商品マスタから項目設定 *******************************
     MOVE     JR3-F10        TO    SYO-F01.
     PERFORM  NFSHOMS-READ-SEC.
     IF   NFSHOMS-INV-FLG = SPACE
          MOVE SYO-F05       TO   CKD-F08
          MOVE SYO-F06       TO   CKD-F09
     ELSE
          MOVE SPACE         TO   CKD-F08
          MOVE SPACE         TO   CKD-F09
     END-IF.
     MOVE     "1"            TO   CKD-F12.
     MOVE     JR3-F02        TO   CKD-F300.
     MOVE     JR3-F09        TO   CKD-F301.
     MOVE     JR3-F04        TO   CKD-F302.
     MOVE     JR3-F03        TO   CKD-F303.
     MOVE     JR3-F12        TO   CKD-F304.
     MOVE     JR3-F13        TO   CKD-F305.
     MOVE     JR3-F14        TO   CKD-F306.
     MOVE     JR3-F16        TO   CKD-F307.
     MOVE     JR3-F17        TO   CKD-F308.
     MOVE     JR3-F18        TO   CKD-F309.
     MOVE     284202         TO   CKD-F43.
*
     IF   NFCKDTF-INV-FLG  =  "INV"
          ADD    1           TO   NFCKDTF-WT-CNT
          WRITE  CKD-REC
     ELSE
          ADD    1           TO   NFCKDTF-RW-CNT
          REWRITE CKD-REC
     END-IF.
*
     PERFORM  NFJYURF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    ナフコ商品マスタ検索
****************************************************************
 NFSHOMS-READ-SEC           SECTION.
     MOVE  "NFSHOMS-READ-SEC" TO S-NAME.
*
     READ  NFSHOMS
       INVALID
         MOVE  "INV"             TO  NFSHOMS-INV-FLG
       NOT INVALID
         MOVE  SPACE             TO  NFSHOMS-INV-FLG
     END-READ.
*
 NFSHOMS-READ-EXIT.
     EXIT.
****************************************************************
*  終了処理                                          3.0       *
****************************************************************
 END-SEC                  SECTION.
     MOVE  "END-SEC"      TO  S-NAME.
* ファイルのＯＰＥＮ
     CLOSE  NFJYURF.
     CLOSE  NFSHOMS.
     CLOSE  NFCKDTF.
*
     DISPLAY "## READ-CNT   =  "  NFJYURF-RD-CNT  UPON CONS.
     DISPLAY "## NFCKDTF WT =  "  NFCKDTF-WT-CNT  UPON CONS.
     DISPLAY "## NFCKDTF RW =  "  NFCKDTF-RW-CNT  UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  SSY3995C   END PROGRAM  >>******************
