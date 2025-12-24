****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　受領返品　　　　　　　　　　　　
*    モジュール名　　　　：　受信状況リスト　　　　　　　　　
*    作成日／作成者　　　：　2017/09/13 INOUE
*    流用元　　　　　　　：　SDE0040L
*    処理概要　　　　　　：　パラメタに合致するレコードより
*    　　　　　　　　　　　　各種取込件数をリスト出力する。
*    更新履歴　　　　　　：
*    　　　　　　　　　　　
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SJR0210L.
 AUTHOR.                   NAV-ASSIST.
 DATE-WRITTEN.             2017/09/13.
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
*受領受信状況ファイル
     SELECT     COMJYOL1   ASSIGN    TO        COMJYOL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       JYO-F01
                                               JYO-F02
                                               JYO-F03
                                               JYO-F04
                           FILE      STATUS    JYO-ST.
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
*受領受信状況ファイル
 FD  COMJYOL1
     LABEL       RECORD    IS        STANDARD.
     COPY        COMJYOL1  OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
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
     03  JYO-ST            PIC X(02).
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
 01  WK-JYO-F03            PIC  9(08)   VALUE ZERO.
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
 01  FG-COMJYOL1-INV       PIC  X(03) VALUE SPACE.
 01  FG-COMJYOL1-END       PIC  X(03) VALUE SPACE.
 01  FG-TOKMS2-INV         PIC  X(03) VALUE SPACE.
 01  SET-FLG               PIC  X(03) VALUE SPACE.
 01  HENKAN-FLG            PIC  X(03) VALUE SPACE.
*帳票出力定義エリア
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SJR0210L".
     02  FILLER              PIC  X(21)  VALUE  SPACE.
     02  FILLER              PIC  N(16)
         CHARACTER  TYPE  IS  YA-22      VALUE
         NC"＜受信状況リスト（受領・返品）＞".
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
     02  MEISAI-1-RET        PIC  N(15)
         CHARACTER  TYPE  IS  YA-22      VALUE
         NC"以下のデータを取り込みました。".
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
         NC"受領データ件数".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(06)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"受領伝票枚数".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(07)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"返品データ件数".
     02  FILLER              PIC  X(04)  VALUE  SPACE.
     02  FILLER              PIC  N(06)
         CHARACTER  TYPE  IS  YA         VALUE
         NC"返品伝票枚数".
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
     02  MAISU01             PIC  N(06)  CHARACTER TYPE IS YA.
     02  MAISU01-RT          PIC  N(01)  VALUE  NC"枚"
                                         CHARACTER TYPE IS YA.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  KENSU02             PIC  N(06)  CHARACTER TYPE IS YA.
     02  KENSU02-RT          PIC  N(01)  VALUE  NC"件"
                                         CHARACTER TYPE IS YA.
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  MAISU02             PIC  N(06)  CHARACTER TYPE IS YA.
     02  MAISU02-RT          PIC  N(01)  VALUE  NC"枚"
                                         CHARACTER TYPE IS YA.
*
 LINKAGE                   SECTION.
 01  PAR-IN-BDATE            PIC 9(08).
 01  PAR-IN-BTIME            PIC 9(04).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PAR-IN-BDATE
                                          PAR-IN-BTIME.
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR   UPON      STA.
     DISPLAY     PRT-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
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
     PERFORM     MAIN-SEC  UNTIL FG-COMJYOL1-END = "END".
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
     OPEN        INPUT     TOKMS2   COMJYOL1.
     OPEN        OUTPUT    PRTF.
*
     DISPLAY  "*** SJR0210L START *** "
              WK-Y   "." WK-M   "." WK-D   " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 INIT-01.
*初期ＲＥＡＤ
     PERFORM     COMJYOL1-START-SEC.
     IF          FG-COMJYOL1-INV NOT = "INV"
                 PERFORM     COMJYOL1-READ-SEC
     END-IF.
     IF        ( FG-COMJYOL1-INV NOT = "INV" ) AND
               ( FG-COMJYOL1-END NOT = "END" )
                 MOVE     JYO-F03    TO   WK-JYO-F03
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
*    印字内容セット
     PERFORM     MEIEDT-SEC.
 MAIN-02.
*    同一取引先処理
     PERFORM     COMJYOL1-READ-SEC.
*    最終レコードＷＲＩＴＥ有無判定
     IF        ( FG-COMJYOL1-END  = "END"         )
                 GO                 TO   MAIN-03
     END-IF.
*    取引先ブレイク判定
     IF        ( JYO-F03 NOT =  WK-JYO-F03 )
                 MOVE    JYO-F03    TO   WK-JYO-F03
                 GO                 TO   MAIN-03
     ELSE
                 GO                 TO   MAIN-01
     END-IF.
*
 MAIN-03.
*    明細ＷＲＩＴＥ
     PERFORM     MEIWRT-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE       PRTF   TOKMS2   COMJYOL1.
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     DISPLAY  "*** SJR0210L END   *** "
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
     IF       ( FG-COMJYOL1-INV = "INV" )  OR
              ( FG-COMJYOL1-END = "END" )
                MOVE    NC"該当するデータはありません　　"
                                TO         MEISAI-1-RET
     END-IF.
     WRITE      P-REC   FROM    MEISAI-1   AFTER  4.
     WRITE      P-REC   FROM    MEISAI-1-1 AFTER  4.
     MOVE       SPACE   TO      P-REC.
     WRITE      P-REC                      AFTER  1.
     IF       ( FG-COMJYOL1-INV = "INV" )  OR
              ( FG-COMJYOL1-END = "END" )
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
                WRITE   P-REC  FROM  MEISAI-1-2   AFTER  2
     END-IF.
*
*    ADD         10        TO        LINE-CNT.
*
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 受領受信状況ファイルＳＴＡＲＴ         *
**********************************************************
 COMJYOL1-START-SEC          SECTION.
*
     MOVE        PAR-IN-BDATE   TO    JYO-F01.
     MOVE        PAR-IN-BTIME   TO    JYO-F02.
     MOVE        ZERO           TO    JYO-F03.
     MOVE        ZERO           TO    JYO-F04.
     START       COMJYOL1  KEY  IS >= JYO-F01
                                      JYO-F02
                                      JYO-F03
                                      JYO-F04
          INVALID
                 MOVE  "INV"    TO    FG-COMJYOL1-INV
          NOT INVALID
                 MOVE  "   "    TO    FG-COMJYOL1-INV
     END-START.
*
 COMJYOL1-START-EXIT.
     EXIT.
**********************************************************
*                 受領受信状況ファイルＲＥＡＤ　         *
**********************************************************
 COMJYOL1-READ-SEC           SECTION.
*
     READ        COMJYOL1
          AT END
                 MOVE  "END"    TO    FG-COMJYOL1-END
                 GO             TO    COMJYOL1-READ-EXIT
          NOT AT END
                 MOVE  "   "    TO    FG-COMJYOL1-END
     END-READ.
*
     IF        ( JYO-F01        =     PAR-IN-BDATE ) AND
               ( JYO-F02        =     PAR-IN-BTIME )
                 CONTINUE
     ELSE
                 MOVE  "END"    TO    FG-COMJYOL1-END
     END-IF.
*
 COMJYOL1-READ-EXIT.
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
     MOVE        JYO-F03              TO      BTORI   TOK-F01.
     PERFORM     RD-TOKMS2-SEC.
     IF          FG-TOKMS2-INV NOT = "INV"
                 MOVE    TOK-F02      TO      TORINAME
     ELSE
                 MOVE    ALL NC"？"   TO      TORINAME
     END-IF.
*取込件数・枚数
     IF       JYO-F04   =  01
*             受領データ件数
              MOVE     JYO-F09         TO  WK-KENSU
              PERFORM  ZENKAKU-SEC
              MOVE     WK-HENKAN-N     TO  KENSU01
*             受領伝票枚数
              MOVE     JYO-F10         TO  WK-KENSU
              PERFORM  ZENKAKU-SEC
              MOVE     WK-HENKAN-N     TO  MAISU01
     END-IF.
     IF       JYO-F04   =  02
*             返品データ件数
              MOVE     JYO-F09         TO  WK-KENSU
              PERFORM  ZENKAKU-SEC
              MOVE     WK-HENKAN-N     TO  KENSU02
*             返品伝票枚数　
              MOVE     JYO-F10         TO  WK-KENSU
              PERFORM  ZENKAKU-SEC
              MOVE     WK-HENKAN-N     TO  MAISU02
     END-IF.
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
     MOVE        NC"枚"    TO        MAISU01-RT   MAISU02-RT.
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