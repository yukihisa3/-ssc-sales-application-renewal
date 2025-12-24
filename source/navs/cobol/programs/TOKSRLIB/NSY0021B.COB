****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭ仕入先統合　　　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭ発注業務                    *
*    モジュール名　　　　：　ＤＣＭ発注基本情報作成            *
*    作成日／更新日　　　：　2021/02/12                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　ＤＣＭ発注受信ＤＴを順読みし、新　*
*                            ＤＣＭ基本情報Ｆを作成する。      *
****************************************************************
**履歴**********************************************************
*    2021/02/12  高橋　　新規作成（ＳＪＨ８７００Ｂ）コピー　　*
*    0000/00/00  　　　　　　　　　　　　　　　　　　　　　　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NSY0021B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   DCMEDIAL   ASSIGN    TO        DA-01-S-DCMEDIAL
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    EDI-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*新ＤＣＭ基本情報Ｆ
     SELECT   DNJOHOF   ASSIGN    TO        DA-01-VI-DNJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-K01   JOH-K02
                                            JOH-K03   JOH-K04
                                            JOH-K05   JOH-K06
                                            JOH-K07   JOH-K08
                        FILE  STATUS   IS   JOH-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
*ルート条件マスタ
     SELECT   JHMRUTL1  ASSIGN    TO        DA-01-VI-JHMRUTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUT-F01   RUT-F02
                                            RUT-F03
                        FILE STATUS    IS   RUT-STATUS.
*商品変換ＴＢＬ
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*ＤＣＭ取引先設定マスタ
     SELECT   DCMTOKF   ASSIGN    TO        DA-01-VI-DCMTOKL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   DTK-F01 DTK-F02
                                                 DTK-F03 DTK-F04
                        FILE      STATUS    IS   DTK-STATUS.
*ＤＣＭ発注件数ファイル
     SELECT   DCMKENF   ASSIGN    TO        DA-01-VI-DCMKENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   KEN-F01 KEN-F02
                                                 KEN-F03
                        FILE      STATUS    IS   KEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    新ＤＣＭ発注受信データ
******************************************************************
 FD  DCMEDIAL
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(3147).
******************************************************************
*    新ＤＣＭ基本情報Ｆ
******************************************************************
 FD  DNJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     DNJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    ルート条件マスタ
******************************************************************
 FD  JHMRUTL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMRUTF   OF        XFDLIB
              JOINING   RUT       PREFIX.
******************************************************************
*    商品変換ＴＢＬ　
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*****************************************************************
*    ＤＣＭ取引先設定マスタ
******************************************************************
 FD  DCMTOKF            LABEL RECORD   IS   STANDARD.
     COPY     DCMTOKF   OF        XFDLIB
              JOINING   DTK       PREFIX.
*****************************************************************
*    ＤＣＭ発注件数ファイル
******************************************************************
 FD  DCMKENF            LABEL RECORD   IS   STANDARD.
     COPY     DCMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   DNHACHED OF XFDLIB  JOINING   HED  AS   PREFIX.
*ヘッダ情報格納領域
     COPY   DNHACMEI OF XFDLIB  JOINING   MEI  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  JOH-CNT                 PIC  9(08)     VALUE  ZERO.
 01  HED-CNT                 PIC  9(08)     VALUE  ZERO.
 01  MEI-CNT                 PIC  9(08)     VALUE  ZERO.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTENMS-CHK-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMRUTL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  DCMHSBF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  DCMTOKF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  DCMKENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-TOK-F81              PIC  X(02)     VALUE  SPACE.
 01  WK-TOK-F95              PIC  X(01)     VALUE  SPACE.
 01  WK-JOH-F17              PIC  9(02)     VALUE  ZERO.
 01  WK-JOH-F304             PIC  X(06)     VALUE  SPACE.
 01  WK-JOH-F21              PIC  9(04)     VALUE  ZERO.
 01  WK-TBL-F04              PIC  X(02)     VALUE  SPACE.
 01  WK-RUTO                 PIC  X(02)     VALUE  SPACE.
 01  KETA-CNT                PIC  9(02)     VALUE  ZERO.
 01  I                       PIC  9(02)     VALUE  ZERO.
 01  WK-TORIHIKISAKI         PIC  9(08)     VALUE  ZERO.
*伝票番号変換（文字⇒文字）
 01  WK-DENNOX             PIC  X(09).
 01  WK-DENNOX-R           REDEFINES  WK-DENNOX.
     03  WK-HEN-DENNOX     OCCURS 9.
       05  WK-HEN-DENNOXX  PIC  X(01).
*伝票番号変換（文字⇒数値）
 01  WK-DENNO              PIC  X(09).
 01  WK-DENNO-R            REDEFINES  WK-DENNO.
     03  WK-HEN-DENNO      PIC  9(09).
*伝票番号変換（文字⇒数値）
 01  WK-DENNO1             PIC  X(09).
 01  WK-DENNO1-R           REDEFINES  WK-DENNO1.
     03  WK-HEN-DENNO1     PIC  X(03).
     03  WK-HEN-DENNO2     PIC  X(06).
*取引先ＣＤ変換（文字⇒数値）
 01  WK-TORICD-HEN         PIC  X(06).
 01  WK-TORICD-HEN-R       REDEFINES  WK-TORICD-HEN.
     03  WK-HEN-TORICD     PIC  9(06).
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  EDI-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  HSB-STATUS        PIC  X(02).
     03  DTK-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NSY0021B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NSY0021B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NSY0021B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE  PARA-JTIME.
 DECLARATIVES.
 FILEERR-SEC01          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMEDIAL.
     MOVE      "DCMEDIAL "   TO   AB-FILE.
     MOVE      EDI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC02          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DNJOHOF.
     MOVE      "DNJOHOF "   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     DISPLAY "JOH-K01 = " JOH-K01  UPON  CONS.
     DISPLAY "JOH-K02 = " JOH-K02  UPON  CONS.
     DISPLAY "JOH-K03 = " JOH-K03  UPON  CONS.
     DISPLAY "JOH-K04 = " JOH-K04  UPON  CONS.
     DISPLAY "JOH-K05 = " JOH-K05  UPON  CONS.
     DISPLAY "JOH-K06 = " JOH-K06  UPON  CONS.
     DISPLAY "JOH-K07 = " JOH-K07  UPON  CONS.
     DISPLAY "JOH-K08 = " JOH-K08  UPON  CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC03          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2  "   TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC04          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC05          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMRUTL1.
     MOVE      "JHMRUTL1"   TO   AB-FILE.
     MOVE      RUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC06          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMTOKF.
     MOVE      "DCMTOKL1"    TO   AB-FILE.
     MOVE      DTK-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC07          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL1 "    TO   AB-FILE.
     MOVE      TBL-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC08          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMKENF.
     MOVE      "DCMKENL1"    TO   AB-FILE.
     MOVE      KEN-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     DCMEDIAL   TOKMS2  JHMRUTL1  TENMS1.
     OPEN     I-O       DNJOHOF    DCMKENF.
     OPEN     INPUT     DCMTOKF    HSHOTBL.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG       RD-CNT.
     MOVE     ZERO      TO        JOH-CNT.
*
     DISPLAY NC"＃バッチ日付" " = " PARA-JDATE  UPON  CONS.
     DISPLAY NC"＃バッチ時刻" " = " PARA-JTIME  UPON  CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*
     PERFORM  DCMEDIAL-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*マスタ関係索引各情報を取得
 MAIN010.
*　商品変換ＴＢＬ索引
     MOVE    WK-TORIHIKISAKI     TO   TBL-F01.
     MOVE    MEI-M02             TO   TBL-F02.
     PERFORM HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG  =  SPACE
         MOVE  TBL-F04           TO   WK-TBL-F04
     ELSE
         MOVE  SPACE             TO   WK-TBL-F04
     END-IF.
 MAIN020.
*　取引先マスタ索引
     MOVE    WK-TORIHIKISAKI     TO   TOK-F01.
     PERFORM TOKMS2-READ-SEC.
     IF  TOKMS2-INV-FLG  =  "INV"
         DISPLAY NC"！！取引先マスタ読込ＮＧ！！" UPON CONS
         MOVE 4000            TO    PROGRAM-STATUS
         DISPLAY "ERR-KEY1 = " TOK-F01  UPON CONS
         STOP  RUN
     ELSE
         MOVE  TOK-F95           TO   WK-TOK-F95
         MOVE  TOK-F81           TO   WK-TOK-F81
     END-IF.
 MAIN030.
*新ＤＣＭ基本情報Ｆ初期化
     MOVE    SPACE               TO   JOH-REC.
     INITIALIZE                       JOH-REC.
*ヘッダ／明細部セット
     MOVE    HED-REC             TO   JOH-K20.
     MOVE    MEI-REC             TO   JOH-K21.
*ＫＥＹ項目セット
*　バッチ日付
     MOVE    PARA-JDATE          TO   JOH-K01.
*　バッチ時刻
     MOVE    PARA-JTIME          TO   JOH-K02.
*　バッチ取引先　　　　
     MOVE    WK-TORIHIKISAKI     TO   JOH-K03.
*　店舗ＣＤ　
     MOVE    HED-F21             TO   JOH-K05.
*　伝票番号　　
     MOVE    HED-F03             TO   JOH-K06.
*　行番号　　　
     MOVE    MEI-M03             TO   JOH-K07.
*　行番号＝１の時、振分場所を取得
     IF   JOH-K07  NOT =  1
          GO                     TO   MAIN040
     END-IF.
*ルート振分判定
     EVALUATE    WK-TOK-F95
        WHEN   "1"   *>ルート条件マスタより
        PERFORM  JHMRUTL1-READ-SEC
        IF  JHMRUTL1-INV-FLG  =  "INV"
            MOVE WK-TOK-F81      TO   WK-RUTO *>代表倉庫
        ELSE
            MOVE RUT-F05         TO   WK-RUTO *>ルート倉庫
        END-IF
        WHEN   "2"   *>商品変換ＴＢＬ
        IF  WK-TBL-F04  =  SPACE
            MOVE WK-TOK-F81      TO   WK-RUTO *>代表倉庫
        ELSE
            MOVE WK-TBL-F04      TO   WK-RUTO *>変換ＴＢＬ
        END-IF
        WHEN  "3"   *>代表倉庫
        MOVE     WK-TOK-F81      TO   WK-RUTO *>代表倉庫
        WHEN  OTHER *>対象外の時
        MOVE     WK-TOK-F81      TO   WK-RUTO *>代表倉庫
     END-EVALUATE.
 MAIN040.
*　倉庫ＣＤ　
     MOVE    WK-RUTO             TO   JOH-K04.
*　納品日　　　
     MOVE    HED-F042            TO   JOH-K08.
*　納品予定作成区分
     MOVE    SPACE               TO   JOH-K09.
*　納品予定作成日
     MOVE    ZERO                TO   JOH-K10.
*　納品予定送信区分
     MOVE    SPACE               TO   JOH-K11.
*　納品予定送信日
     MOVE    ZERO                TO   JOH-K12.
 MAIN050.
*新ＤＣＭ基本情報Ｆレコード作成
     WRITE   JOH-REC.
     ADD     1                   TO   JOH-CNT.
*ＤＣＭ発注件数ファイル更新　　　
     PERFORM DCMKENF-WT-SEC.
*
     PERFORM DCMEDIAL-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル読込　　　　　　　　　　　　　　　　　　*
****************************************************************
 DCMEDIAL-READ-SEC      SECTION.
*
     MOVE "DCMEDIAL-READ-SEC" TO       S-NAME.
*
     READ     DCMEDIAL
              AT END
              MOVE     9      TO    END-FG
              GO              TO    DCMEDIAL-READ-EXIT
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*
 DCMEDIAL-READ-010.
     IF   RD-CNT(6:3) = "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 DCMEDIAL-READ-020.
     IF  EDI-01  =  "HD"
         MOVE    SPACE        TO    HED-REC
         INITIALIZE                 HED-REC
         MOVE    EDI-REC      TO    HED-REC
         ADD     1            TO    HED-CNT
         GO                   TO    DCMEDIAL-READ-SEC
     ELSE
         MOVE    SPACE        TO    MEI-REC
         INITIALIZE                 MEI-REC
         MOVE    EDI-REC      TO    MEI-REC
         ADD     1            TO    MEI-CNT
     END-IF.
*カーマのみ実行
     IF  HED-F304  NOT =  "013938"
         GO                   TO    DCMEDIAL-READ-SEC
     END-IF.
 DCMEDIAL-READ-030.
*ホーマックの場合は、マスタより取得すること
     IF  HED-F304  =  "000880"
     OR  HED-F304  =  "001427"
         CONTINUE
     ELSE
         GO                   TO    DCMEDIAL-READ-040
     END-IF.
*ＤＣＭ取引先変換マスタ読込　サカタ様と協議し現時点では未使用
     MOVE     HED-F27         TO    DTK-F01
     MOVE     HED-F201        TO    DTK-F02
     MOVE     HED-F304        TO    DTK-F03
     MOVE     HED-F17         TO    DTK-F04
     PERFORM DCMTOKF-READ-SEC.
     IF  DCMTOKF-INV-FLG = "INV"
         DISPLAY NC"！！ＤＣＭ取引先変換ＮＧ！！" UPON CONS
         MOVE 4000            TO    PROGRAM-STATUS
         DISPLAY "ERR-KEY1 = " DTK-F01  UPON CONS
         DISPLAY "ERR-KEY2 = " DTK-F02  UPON CONS
         DISPLAY "ERR-KEY3 = " DTK-F03  UPON CONS
         DISPLAY "ERR-KEY4 = " DTK-F04  UPON CONS
         STOP  RUN
     ELSE
         MOVE  DTK-F05        TO    WK-TORIHIKISAKI
     END-IF.
     IF  HED-F304  =  "000880"
     OR  HED-F304  =  "001427"
         GO                   TO    DCMEDIAL-READ-EXIT
     END-IF.
 DCMEDIAL-READ-040.
*店舗マスタを索引し、取引先ＣＤをＮＡＶＳ内取引先を生成
     MOVE      HED-F304       TO    WK-TORICD-HEN.
     MOVE      WK-HEN-TORICD  TO    TEN-F52.
     MOVE      HED-F21        TO    TEN-F011.
     PERFORM   HTENMS-READ-SEC.
     EVALUATE  WK-HEN-TORICD  ALSO  HTENMS-CHK-FLG
         WHEN  13938          ALSO  "CHK"
               MOVE  139381   TO    WK-TORIHIKISAKI
         WHEN  17137          ALSO  "CHK"
               MOVE  171371   TO    WK-TORIHIKISAKI
         WHEN  173            ALSO  SPACE
               MOVE  1731     TO    WK-TORIHIKISAKI
         WHEN  173            ALSO  "CHK"
               MOVE  1732     TO    WK-TORIHIKISAKI
         WHEN  760            ALSO  SPACE
               MOVE  7601     TO    WK-TORIHIKISAKI
         WHEN  760            ALSO  "CHK"
               MOVE  7602     TO    WK-TORIHIKISAKI
         WHEN  100403         ALSO  "CHK"
               MOVE  100404   TO    WK-TORIHIKISAKI
         WHEN  100441         ALSO  "CHK"
               MOVE  100442   TO    WK-TORIHIKISAKI
         WHEN  100427         ALSO  "CHK"
               MOVE  100428   TO    WK-TORIHIKISAKI
         WHEN  OTHER
               MOVE  WK-HEN-TORICD  TO  WK-TORIHIKISAKI
     END-EVALUATE.
*
 DCMEDIAL-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　取引先マスタ読込　　　　　　　　　　　　　　　　*
****************************************************************
 TOKMS2-READ-SEC  SECTION.
*
     READ    TOKMS2
             INVALID
             MOVE  "INV"   TO    TOKMS2-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　店舗マスタ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 HTENMS-READ-SEC  SECTION.
*
     MOVE    SPACE         TO    HTENMS-CHK-FLG.
     READ    TENMS1
             INVALID
             MOVE  "INV"   TO    HTENMS-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    HTENMS-INV-FLG
     END-READ.
*
     IF  HTENMS-INV-FLG = "INV"
         MOVE  SPACE       TO    HTENMS-CHK-FLG
     ELSE
         IF  TEN-F73 = SPACE
*************本社　ダイキ
             MOVE SPACE    TO    HTENMS-CHK-FLG
         ELSE
*************九州　ダイキ（サンコー）
             MOVE "CHK"    TO    HTENMS-CHK-FLG
         END-IF
     END-IF.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ルートマスタ読込　　　　　　　　　　　　　　　　*
****************************************************************
 JHMRUTL1-READ-SEC  SECTION.
*
     MOVE WK-TORIHIKISAKI  TO        RUT-F01.
     MOVE SPACE            TO        RUT-F02.
     IF  WK-TORIHIKISAKI  =  1731  OR  1732  OR  7601  OR  7602
         MOVE HED-F14      TO        RUT-F02
     END-IF.
     MOVE HED-F31(1:3)     TO        RUT-F03.
     READ     JHMRUTL1
         INVALID
           MOVE  "INV"     TO        JHMRUTL1-INV-FLG
         NOT  INVALID
           MOVE  SPACE     TO        JHMRUTL1-INV-FLG
     END-READ.
*
 JHMRUTL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　新ＤＣＭ取引先設定マスタ読込　　　　　　　　　　*
****************************************************************
 DCMTOKF-READ-SEC SECTION.
*
     READ    DCMTOKF
             INVALID
             MOVE  "INV"   TO    DCMTOKF-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    DCMTOKF-INV-FLG
     END-READ.
*
 DCMTOKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　商品変換ＴＢＬ読込
****************************************************************
 HSHOTBL-READ-SEC SECTION.
*
     READ    HSHOTBL
             INVALID
             MOVE  "INV"   TO    HSHOTBL-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＤＣＭ発注件数ファイル
****************************************************************
 DCMKENF-WT-SEC   SECTION.
*
     MOVE    JOH-K01       TO    KEN-F01.
     MOVE    JOH-K02       TO    KEN-F02.
     MOVE    JOH-K03       TO    KEN-F03.
     READ    DCMKENF
             INVALID
             MOVE  "INV"   TO    DCMKENF-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    DCMKENF-INV-FLG
     END-READ.
*
     IF  DCMKENF-INV-FLG  =  "INV"
         MOVE      SPACE   TO    KEN-REC
         INITIALIZE              KEN-REC
         MOVE      JOH-K01 TO    KEN-F01
         MOVE      JOH-K02 TO    KEN-F02
         MOVE      JOH-K03 TO    KEN-F03
         MOVE      1       TO    KEN-F04
         WRITE  KEN-REC
     ELSE
         ADD       1        TO   KEN-F04
         REWRITE  KEN-REC
     END-IF.
*
 DCMKENF-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     DCMEDIAL  DNJOHOF  TOKMS2  TENMS1  HSHOTBL
               JHMRUTL1  DCMTOKF  DCMKENF.
*
     DISPLAY NC"＃受信Ｆ"     "     = "   RD-CNT   UPON  CONS.
     DISPLAY NC"＃基本情報Ｆ" " = "       JOH-CNT  UPON  CONS.
     DISPLAY NC"＃ヘッダ部"   "   = "     HED-CNT  UPON  CONS.
     DISPLAY NC"＃明細部"     "     = "   MEI-CNT  UPON  CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
