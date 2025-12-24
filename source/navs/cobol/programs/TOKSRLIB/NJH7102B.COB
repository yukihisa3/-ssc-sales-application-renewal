****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　ジョイフル本田　発注・受領　　　
*    モジュール名　　　　：　受信確認リスト　　　　　　　　　
*    作成日／作成者　　　：　2022/10/31 INOUE
*    流用元　　　　　　　：　SJR0210L
*    処理概要　　　　　　：　受信データを種別毎に振り分け。
*    　　　　　　　　　　　　各件数をリスト出力する。
*    更新履歴　　　　　　：
*    　　　　　　　　　　　
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               NJH7102B.
 AUTHOR.                   NAV-ASSIST.
 DATE-WRITTEN.             2022/10/31.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA
     YA          IS        YA
     YB          IS        YB
     YA-22       IS        YA-22
     YB-21       IS        YB-21
     YB-22       IS        YB-22
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*流通ＢＭＳ受信データ
     SELECT     ONLNWJHD   ASSIGN    TO        ONLNWJHD
                           ORGANIZATION        SEQUENTIAL
                           ACCESS    MODE      SEQUENTIAL
                           FILE      STATUS    INP-ST.
*発注データ
     SELECT     ONLHONDA   ASSIGN    TO        ONLHONDA
                           ORGANIZATION        SEQUENTIAL
                           ACCESS    MODE      SEQUENTIAL
                           FILE      STATUS    HAC-ST.
*仕入実績データ
     SELECT     HDJISKSF   ASSIGN    TO        HDJISKSF
                           ORGANIZATION        SEQUENTIAL
                           ACCESS    MODE      SEQUENTIAL
                           FILE      STATUS    SIR-ST.
*取引先マスタ
     SELECT     TOKMS2     ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
* プリンター
     SELECT     PRTF       ASSIGN    TO        LP-04.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*
*流通ＢＭＳ受信データ
 FD  ONLNWJHD
     LABEL       RECORD    IS        STANDARD
     BLOCK       CONTAINS  1         RECORDS.
*    JOINING     INP       AS        PREFIX.
 01  INP-REC.
     03  FILLER                      PIC  X(256).
*発注データ
 FD  ONLHONDA
     LABEL       RECORD    IS        STANDARD
     BLOCK       CONTAINS  1         RECORDS.
*    JOINING     HAC       AS        PREFIX.
 01  HAC-REC.
     03  FILLER                      PIC  X(256).
*仕入実績データ
 FD  HDJISKSF
     LABEL       RECORD    IS        STANDARD
     BLOCK       CONTAINS  1         RECORDS.
*    JOINING     SIR       AS        PREFIX.
 01  SIR-REC.
     03  FILLER                      PIC  X(128).
*取引先マスタ
 FD  TOKMS2
     LABEL       RECORD    IS        STANDARD.
     COPY        TOKMS2    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*プリンター
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
 01  FILE-STATUS.
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  INP-ST            PIC X(02).
     03  HAC-ST            PIC X(02).
     03  SIR-ST            PIC X(02).
     03  TOK-ST            PIC X(02).
*年度
 01  YYYY.
     03  YY1               PIC  9(02).
     03  YY2               PIC  9(02).
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-NYUKIN         PIC 9(02) VALUE     ZERO.
*
 01  CNT-AREA.
     03  INP-CNT           PIC  9(07)  VALUE  ZERO.
     03  HAC-CNT           PIC  9(07)  VALUE  ZERO.
     03  SIR-CNT           PIC  9(07)  VALUE  ZERO.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
     03  SYS-TIME   REDEFINES  WK-TIME.
         05  SYS-HH         PIC  9(02).
         05  SYS-MN         PIC  9(02).
         05  SYS-SS         PIC  9(02).
         05  SYS-MS         PIC  9(02).
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  TOK-ERR           PIC N(10) VALUE
                        NC"取引先マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
     03  INP-ERR           PIC N(10) VALUE
                        NC"受信データエラー".
     03  HAC-ERR           PIC N(10) VALUE
                        NC"発注データエラー".
     03  SIR-ERR           PIC N(10) VALUE
                        NC"仕入実績　エラー".
*帳票表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*帳票表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*退避エリア
 01  WK-BMNCD-BK           PIC  X(04).
 01  WK-TANCD-BK           PIC  X(02).
 01  WK-TANNM-BK           PIC  N(10).
 01  WK-TORCD-BK           PIC  X(08).
 01  WK-TORNM-BK           PIC  N(15).
 01  WK-INP-F03            PIC  9(08)   VALUE ZERO.
*01  WK-HIT                PIC  X(03)   VALUE SPACE.
*変換エリア
 01  TB-CVT-SU.
     03  TB-CVT-SU          PIC  X(11)  VALUE
         "0123456789 ".
     03  TB-CVT-SUR  REDEFINES TB-CVT-SU
                            PIC  X(01)  OCCURS 11.

     03  TB-CVT-SU-N        PIC  N(11)  VALUE
       NC"０１２３４５６７８９　".
     03  TB-CVT-SU-NR  REDEFINES TB-CVT-SU-N
                            PIC  N(01)  OCCURS 11.

 01  WK-KENSU-X.
     03  WK-KENSU          PIC  9(05) VALUE  ZERO.
 01  WK-HENKAN.
     03  WK-HENKAN-N       PIC  N(05).
     03  WK-HENKAN-NR  REDEFINES  WK-HENKAN-N
                           PIC  N(01)  OCCURS 5.
 01  WK-SHUKA-X.
     03  WK-SHUKA-G        PIC  9(07) VALUE  ZERO.
 01  WK-NYUKA-X.
     03  WK-NYUKA-G        PIC  9(07) VALUE  ZERO.
*更新範囲
 01  TRND-DT.
     03  TRND-DATE         PIC  9(08).
     03  TRND-TIME         PIC  9(06).
 01  TO-DT.
     03  TO-DATE           PIC  9(08).
     03  TO-TIME           PIC  9(06).
*
 01  IX                    PIC  9(04) VALUE 0.
 01  READ-CNT              PIC  9(07) VALUE 0.
 01  IN-CNT                PIC  9(07) VALUE 0.
 01  FG-ONLNWJHD-INV       PIC  X(03) VALUE SPACE.
 01  FG-ONLNWJHD-END       PIC  X(03) VALUE SPACE.
 01  FG-TOKMS2-INV         PIC  X(03) VALUE SPACE.
 01  SET-FLG               PIC  X(03) VALUE SPACE.
 01  HENKAN-FLG            PIC  X(03) VALUE SPACE.
*帳票出力定義エリア
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "NJH7102B".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(16)
         CHARACTER  TYPE  IS  YA-22      VALUE
         NC"＜ジョイフル本田受信確認リスト＞".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  SYSYY               PIC  9999.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA    VALUE
         NC"年".
     02  SYSMM               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA    VALUE
         NC"月".
     02  SYSDD               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA    VALUE
         NC"日".
     02  FILLER              PIC  X(02) VALUE  SPACE.
     02  LPAGE               PIC  ZZ9.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA    VALUE
         NC"頁".
****  見出し行２***
 01  MIDASI-2.
     02  FILLER              PIC  X(116) VALUE  SPACE.
     02  TIMEHH              PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA    VALUE
         NC"：".
     02  TIMEMM              PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA    VALUE
         NC"：".
     02  TIMESS              PIC  99.
****  線１           ****
 01  HASEN-1.
     02  FILLER              PIC  X(136) VALUE  ALL "-".
****  線２           ****
 01  HASEN-2.
     02  FILLER              PIC  X(27) VALUE  SPACE.
     02  FILLER              PIC  X(39) VALUE  ALL "-".

****  明細行１               ****
 01  MEISAI-1.
     02  FILLER              PIC  X(32)  VALUE  SPACE.
     02  MEISAI-1-RET        PIC  N(14)
         CHARACTER  TYPE  IS  YA-22      VALUE
         NC"以下のデータを受信しました。".
****  明細行１-1 項目タイトル****
 01  MEISAI-1-1.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(05)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"バッチ日付".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(02)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"時刻".
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"取引先".
     02  FILLER              PIC  X(36)  VALUE  SPACE.
     02  FILLER              PIC  N(07)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"発注データ件数".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(06)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"仕入実績件数".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
**** 明細行１-2 データ内容  ****
 01  MEISAI-1-2.
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  BDATE               PIC  X(10).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  BTIME               PIC  X(05).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  BTORI               PIC  9(08).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  TORINAME            PIC  N(15)  CHARACTER TYPE IS YA.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  KENSU01             PIC  N(06)  CHARACTER TYPE IS YA.
     02  KENSU01-RT          PIC  N(01)  VALUE  NC"件"
                                         CHARACTER TYPE IS YA.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  KENSU02             PIC  N(06)  CHARACTER TYPE IS YA.
     02  KENSU02-RT          PIC  N(01)  VALUE  NC"件"
                                         CHARACTER TYPE IS YA.
*
 LINKAGE                   SECTION.
 01  PAR-IN-BDATE            PIC 9(08).
 01  PAR-IN-BTIME            PIC 9(04).
 01  PAR-IN-TOKCD            PIC 9(08).
 01  PAR-OUT-KENSU1          PIC 9(07).
 01  PAR-OUT-KENSU2          PIC 9(07).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PAR-IN-BDATE
                                          PAR-IN-BTIME
                                          PAR-IN-TOKCD
                                          PAR-OUT-KENSU1
                                          PAR-OUT-KENSU2.
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 INP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ONLNWJHD.
     DISPLAY     INP-ERR   UPON      STA.
     DISPLAY     INP-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 HAC-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ONLHONDA.
     DISPLAY     HAC-ERR   UPON      STA.
     DISPLAY     HAC-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SIR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HDJISKSF.
     DISPLAY     SIR-ERR   UPON      STA.
     DISPLAY     SIR-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TOKMS2.
     DISPLAY     TOK-ERR   UPON      STA.
     DISPLAY     TOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL FG-ONLNWJHD-END = "END".
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     MOVE        ZERO      TO        PAGE-CNT.
     MOVE        53        TO        LINE-CNT.
*
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
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     OPEN        INPUT     TOKMS2   ONLNWJHD.
     OPEN        OUTPUT    PRTF     ONLHONDA   HDJISKSF.
*
     DISPLAY  "*** NJH7102B START *** "
              WK-Y   "." WK-M   "." WK-D   " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 INIT-01.
*初期ＲＥＡＤ
     PERFORM     ONLNWJHD-READ-SEC.
     IF          FG-ONLNWJHD-INV  = "END"
                 DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
                 DISPLAY NC"＊　対象データなし　＊" UPON CONS
                 DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊" UPON CONS
*                GO      TO     INIT-EXIT
     END-IF.
 INIT-02.
     PERFORM     HEAD-SET-SEC.
     PERFORM     MIDA-SEC.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
*
 MAIN-01.
*    種別判定
     IF       INP-REC(1:2)     = "01"
              MOVE    INP-REC          TO   HAC-REC
              WRITE   HAC-REC
              ADD     1                TO   HAC-CNT
***           GO                       TO   MAIN-03
     END-IF.
     IF       INP-REC(1:2)     = "03"
              MOVE    INP-REC(1:128)   TO   SIR-REC
              WRITE   SIR-REC
              ADD     1                TO   SIR-CNT
***           GO                       TO   MAIN-03
     END-IF.
     IF       INP-REC(129:2)   = "03"
              MOVE    INP-REC(129:128) TO   SIR-REC
              WRITE   SIR-REC
              ADD     1                TO   SIR-CNT
***           GO                       TO   MAIN-03
     END-IF.
*
 MAIN-03.
*
     PERFORM     ONLNWJHD-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
*    印字内容セット
     PERFORM     MEIEDT-SEC.
*    明細ＷＲＩＴＥ
     PERFORM     MEIWRT-SEC.
*
*    パラメタセット
     MOVE        HAC-CNT        TO     PAR-OUT-KENSU1.
     MOVE        SIR-CNT        TO     PAR-OUT-KENSU2.
     DISPLAY     NC"発注　　　件数＝" HAC-CNT UPON CONS.
     DISPLAY     NC"仕入実績　件数＝" SIR-CNT UPON CONS.
*
     CLOSE       PRTF   TOKMS2   ONLNWJHD  ONLHONDA  HDJISKSF.
*
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     DISPLAY  "*** NJH7102B END   *** "
              WK-Y   "." WK-M   "." WK-D   " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-EXIT.
     EXIT.
**********************************************************
*                 ヘッダデータ編集                      *
**********************************************************
 HEAD-SET-SEC                  SECTION.
*
     MOVE        WK-YS     TO        YY1.
     MOVE        WK-Y      TO        YY2.
     MOVE        YYYY      TO        SYSYY.
     MOVE        WK-M      TO        SYSMM.
     MOVE        WK-D      TO        SYSDD.
     MOVE        WK-TIME(1:2)  TO    TIMEHH.
     MOVE        WK-TIME(3:2)  TO    TIMEMM.
     MOVE        WK-TIME(5:2)  TO    TIMESS.
*
*
 HEAD-SET-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 MIDA-SEC                  SECTION.
*
     IF  PAGE-CNT  >  ZERO
         MOVE       SPACE   TO      P-REC
         WRITE      P-REC   AFTER   PAGE
     END-IF.
*
     ADD         1         TO        PAGE-CNT.
     MOVE        ZERO      TO        LINE-CNT.
     MOVE        PAGE-CNT  TO        LPAGE.
**************
*帳票書き出し*
**************
     MOVE       SPACE   TO      P-REC.
     WRITE      P-REC                      AFTER  2.
     WRITE      P-REC   FROM    MIDASI-1   AFTER  1.
     WRITE      P-REC   FROM    MIDASI-2   AFTER  1.
     WRITE      P-REC   FROM    HASEN-1    AFTER  1.
     IF         FG-ONLNWJHD-END = "END"
                MOVE    NC"該当するデータはありません　　"
                                TO         MEISAI-1-RET
     END-IF.
     WRITE      P-REC   FROM    MEISAI-1   AFTER  4.
     WRITE      P-REC   FROM    MEISAI-1-1 AFTER  4.
     MOVE       SPACE   TO      P-REC.
     WRITE      P-REC                      AFTER  1.
     IF         FG-ONLNWJHD-END = "END"
*               バッチ日付
                MOVE    PAR-IN-BDATE(1:4)  TO     BDATE(1:4)
                MOVE    "/"                TO     BDATE(5:1)
                MOVE    PAR-IN-BDATE(5:2)  TO     BDATE(6:2)
                MOVE    "/"                TO     BDATE(8:1)
                MOVE    PAR-IN-BDATE(7:2)  TO     BDATE(9:2)
*               バッチ時刻
                MOVE    PAR-IN-BTIME(1:2)  TO     BTIME(1:2)
                MOVE    ":"                TO     BTIME(3:1)
                MOVE    PAR-IN-BTIME(3:2)  TO     BTIME(4:2)
*****           WRITE   P-REC  FROM  MEISAI-1-2   AFTER  2
     END-IF.
*
*    ADD         10        TO        LINE-CNT.
*
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 流通ＢＭＳ受信データＳＴＡＲＴ         *
**********************************************************
*ONLNWJHD-START-SEC          SECTION.
*
*    MOVE        PAR-IN-BDATE   TO    INP-F01.
*    MOVE        PAR-IN-BTIME   TO    INP-F02.
*    MOVE        ZERO           TO    INP-F03.
*    MOVE        ZERO           TO    INP-F04.
*    START       ONLNWJHD  KEY  IS >= INP-F01
*                                     INP-F02
*                                     INP-F03
*                                     INP-F04
*         INVALID
*                MOVE  "INV"    TO    FG-ONLNWJHD-INV
*         NOT INVALID
*                MOVE  "   "    TO    FG-ONLNWJHD-INV
*    END-START.
*
*ONLNWJHD-START-EXIT.
*    EXIT.
**********************************************************
*                 流通ＢＭＳ受信データＲＥＡＤ　         *
**********************************************************
 ONLNWJHD-READ-SEC           SECTION.
*
     READ        ONLNWJHD
          AT END
                 MOVE  "END"    TO    FG-ONLNWJHD-END
                 GO             TO    ONLNWJHD-READ-EXIT
          NOT AT END
                 MOVE  "   "    TO    FG-ONLNWJHD-END
     END-READ.
*
*    IF        ( INP-F01        =     PAR-IN-BDATE ) AND
*              ( INP-F02        =     PAR-IN-BTIME )
*                CONTINUE
*    ELSE
*                MOVE  "END"    TO    FG-ONLNWJHD-END
*    END-IF.
*
 ONLNWJHD-READ-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
*バッチ日付
     MOVE        PAR-IN-BDATE(1:4)    TO      BDATE(1:4).
     MOVE        "/"                  TO      BDATE(5:1).
     MOVE        PAR-IN-BDATE(5:2)    TO      BDATE(6:2).
     MOVE        "/"                  TO      BDATE(8:1).
     MOVE        PAR-IN-BDATE(7:2)    TO      BDATE(9:2).
*バッチ時刻
     MOVE        PAR-IN-BTIME(1:2)    TO      BTIME(1:2).
     MOVE        ":"                  TO      BTIME(3:1).
     MOVE        PAR-IN-BTIME(3:2)    TO      BTIME(4:2).
*取引先
     MOVE        PAR-IN-TOKCD         TO      BTORI   TOK-F01.
*T
*    DISPLAY "BTORI=" BTORI UPON CONS.
*T
     PERFORM     RD-TOKMS2-SEC.
     IF          FG-TOKMS2-INV NOT = "INV"
                 MOVE    TOK-F02      TO      TORINAME
     ELSE
                 MOVE    ALL NC"？"   TO      TORINAME
     END-IF.
*件数
*    発注データ件数
     MOVE        HAC-CNT               TO  WK-KENSU.
     PERFORM     ZENKAKU-SEC.
     MOVE        WK-HENKAN-N           TO  KENSU01.
*    仕入実績データ件数
     MOVE        SIR-CNT               TO  WK-KENSU.
     PERFORM     ZENKAKU-SEC.
     MOVE        WK-HENKAN-N           TO  KENSU02.
*
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し                  *
**********************************************************
 MEIWRT-SEC                   SECTION.
*
*    ADD        1          TO        IN-CNT.
**************
*帳票書き出し*
**************
     WRITE       P-REC     FROM      MEISAI-1-2   AFTER    2.
     MOVE        SPACE     TO        MEISAI-1-2.
     MOVE        NC"件"    TO        KENSU01-RT   KENSU02-RT.
*----MOVE        NC"枚"    TO        MAISU01-RT   MAISU02-RT.
*
*
*    ADD         2         TO        LINE-CNT.
*
 MEIWRT-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-TOKMS2-SEC          SECTION.

     READ  TOKMS2
       INVALID
         MOVE  "INV"             TO  FG-TOKMS2-INV
       NOT INVALID
         MOVE  "   "             TO  FG-TOKMS2-INV
     END-READ.

 RD-TOKMS2-EXIT.
     EXIT.
****************************************************************
*    全角変換　　　                                          *
****************************************************************
 ZENKAKU-SEC          SECTION.
* 日本語変換する。
     MOVE   SPACE  TO  HENKAN-FLG.
     PERFORM  VARYING IX  FROM 1 BY 1 UNTIL   IX > 5
     IF ( WK-KENSU-X(IX:1) NOT = 0 ) AND
        ( WK-KENSU-X(IX:1) NOT = SPACE )
         MOVE "SET"   TO  HENKAN-FLG
     END-IF
         EVALUATE    WK-KENSU-X(IX:1)
             WHEN    0
                 IF HENKAN-FLG = "SET"
                     MOVE NC"０"  TO   WK-HENKAN-NR(IX)
                 ELSE
                     MOVE NC"　"  TO   WK-HENKAN-NR(IX)
                 END-IF
             WHEN    1
                 MOVE NC"１"  TO   WK-HENKAN-NR(IX)
             WHEN    2
                 MOVE NC"２"  TO   WK-HENKAN-NR(IX)
             WHEN    3
                 MOVE NC"３"  TO   WK-HENKAN-NR(IX)
             WHEN    4
                 MOVE NC"４"  TO   WK-HENKAN-NR(IX)
             WHEN    5
                 MOVE NC"５"  TO   WK-HENKAN-NR(IX)
             WHEN    6
                 MOVE NC"６"  TO   WK-HENKAN-NR(IX)
             WHEN    7
                 MOVE NC"７"  TO   WK-HENKAN-NR(IX)
             WHEN    8
                 MOVE NC"８"  TO   WK-HENKAN-NR(IX)
             WHEN    9
                 MOVE NC"９"  TO   WK-HENKAN-NR(IX)
         END-EVALUATE
     END-PERFORM.
 ZENKAKU-EXIT.
     EXIT.
