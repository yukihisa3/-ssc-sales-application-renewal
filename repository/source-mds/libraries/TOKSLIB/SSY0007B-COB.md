# SSY0007B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY0007B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　発注集計表                        *
*    モジュール名　　　　：　発注集計データ分類集計            *
*    作成日／更新日　　　：　99/10/04                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注集計ワークを読み、サカタ商品  *
*                            コードで、各分類毎に、数量、原価  *
*                            金額、売価金額を集計し、発注集計  *
*                            ワーク分類へ出力する。            *
*    2004/05/28 部門を追加（落花生・一寸そらまめ）             *
*    2007/02/15 分類集計方法を変更　　　　　　　　　　　　　　 *
*    2007/06/15 分類集計方法を変更　　　　　　　　　　　　　　 *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY0007B.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/10/04.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*発注集計ワークファイル
     SELECT   SHWHACF   ASSIGN    TO        DA-01-S-SHWHACF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HACF-STATUS.
*発注集計ワーク（分類）
     SELECT   SHWHACBF  ASSIGN    TO        DA-01-S-SHWHACBF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HACB-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    発注集計ワークファイル
******************************************************************
 FD  SHWHACF            BLOCK     CONTAINS  5    RECORDS.
     COPY     SHWHACF   OF        XFDLIB
              JOINING   HACF  AS  PREFIX.
*
******************************************************************
*    発注集計ワーク（分類）
******************************************************************
 FD  SHWHACBF           BLOCK     CONTAINS  5    RECORDS.
     COPY     SHWHACF   OF        XFDLIB
              JOINING   HACB  AS  PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
*
 01  WK-AREA.
     03  WK-JDATE            PIC  9(08)     VALUE  ZERO.
     03  WK-JTIME            PIC  9(04)     VALUE  ZERO.
     03  WK-TORICD           PIC  9(08)     VALUE  ZERO.
     03  WK-KIGYO            PIC  X(15)     VALUE  SPACE.
     03  WK-SYUKEI           OCCURS    10.
       05  WK-CNT            PIC  9(09)     VALUE  ZERO.
       05  WK-HACSUU         PIC S9(09)V99  VALUE  ZERO.
       05  WK-TEISUU         PIC S9(09)V99  VALUE  ZERO.
       05  WK-GENKIN         PIC S9(11)     VALUE  ZERO.
       05  WK-BAIKIN         PIC S9(11)     VALUE  ZERO.
       05  WK-HACF-REC       PIC  X(200)    VALUE  SPACE.
*
 01  WK-ST.
     03  HACF-STATUS       PIC  X(02).
     03  HACB-STATUS       PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY0007B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY0007B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY0007B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHWHACF.
     MOVE      "SHWHACF "   TO   AB-FILE.
     MOVE      HACF-STATUS  TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHWHACBF.
     MOVE      "SHWHACBF"   TO   AB-FILE.
     MOVE      HACB-STATUS  TO   AB-STS.
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
     OPEN     INPUT     SHWHACF.
     OPEN     OUTPUT    SHWHACBF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     INITIALIZE                   WK-AREA.
*
     READ     SHWHACF
         AT END    MOVE      9         TO   END-FG
         NOT AT END
                   ADD       1         TO   RD-CNT
                   MOVE      HACF-F011 TO   WK-JDATE
                   MOVE      HACF-F012 TO   WK-JTIME
                   MOVE      HACF-F013 TO   WK-TORICD
                   MOVE      HACF-F17  TO   WK-KIGYO
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
     MOVE     ZERO                TO   IX.
*    分類０１（実咲　花）
     IF    ( "00905000"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00909999" )
              MOVE      1         TO   IX
     END-IF.
*    分類０２（実咲　野菜）
     IF    ( "00920000"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00928999" )
              MOVE      2         TO   IX
     END-IF.
     IF    ( "00970400"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00970999" )
              MOVE      2         TO   IX
     END-IF.
*    分類０３（営利）
     IF    ( "00000000"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00099999" )
              MOVE      3         TO   IX
     END-IF.
     IF    ( "00949000"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00949999" )
              MOVE      3         TO   IX
     END-IF.
*    分類０４（球根）
     IF    ( "00300000"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00399999" )
              MOVE      4         TO   IX
     END-IF.
*    分類０５（資材）
     IF    ( "00500000"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00899999" )
              MOVE      5         TO   IX
     END-IF.
*    分類０６（植物）
     IF    ( "00950000"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00959999" )
              MOVE      6         TO   IX
     END-IF.
*    分類０７（芝種）
     IF    ( "00155120"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00155120" )
              MOVE      7         TO   IX
     END-IF.
*    分類０８（球根　特販）
     IF    ( "00959000"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00959999" )
              MOVE      8         TO   IX
     END-IF.
*****分類０９（落花生・一寸そらまめ）
*    IF    ( "00940000"     <=    HACF-F081 ) AND
*          (  HACF-F081     <=   "00949999" )
*             MOVE      9         TO   IX
*****END-IF.
*****分類０９（一寸そらまめ）2007/06/15 NAV 追加
     IF    ( "00100100"     <=    HACF-F081 ) AND
           (  HACF-F081     <=   "00100100" )
              MOVE      9         TO   IX
     END-IF.
*    分類９９（別部署商品）
     IF       IX             =    ZERO
              MOVE      10        TO   IX
     END-IF.
*
     ADD      1              TO   WK-CNT(IX).
     ADD      HACF-F13       TO   WK-HACSUU(IX).
     ADD      HACF-F14       TO   WK-TEISUU(IX).
     ADD      HACF-F18       TO   WK-GENKIN(IX).
     ADD      HACF-F19       TO   WK-BAIKIN(IX).
*
     READ     SHWHACF
         AT END    MOVE      9         TO   END-FG
         NOT AT END
                   ADD       1         TO   RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC      SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     IF  RD-CNT    >         ZERO
         PERFORM   VARYING   IX   FROM  1  BY  1  UNTIL  IX > 10
              IF   WK-CNT(IX)     >    ZERO
                   MOVE      SPACE          TO   HACB-REC
                   INITIALIZE                    HACB-REC
                   MOVE      WK-JDATE       TO   HACB-F011
                   MOVE      WK-JTIME       TO   HACB-F012
                   MOVE      WK-TORICD      TO   HACB-F013
                   MOVE      IX             TO   HACB-F04
                   IF        IX   =    10
                        MOVE     "99"       TO   HACB-F04
                   END-IF
                   MOVE      WK-HACSUU(IX)  TO   HACB-F13
                   MOVE      WK-TEISUU(IX)  TO   HACB-F14
                   MOVE      WK-KIGYO       TO   HACB-F17
                   MOVE      WK-GENKIN(IX)  TO   HACB-F18
                   MOVE      WK-BAIKIN(IX)  TO   HACB-F19
                   WRITE     HACB-REC
                   ADD       1              TO   WRT-CNT
              END-IF
         END-PERFORM
     END-IF.
     MOVE     RD-CNT    TO        IN-CNT.
     MOVE     WRT-CNT   TO        OUT-CNT.
     DISPLAY  MSG-IN    UPON CONS.
     DISPLAY  MSG-OUT   UPON CONS.
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    SHWHACF  SHWHACBF.
*
     STOP     RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
