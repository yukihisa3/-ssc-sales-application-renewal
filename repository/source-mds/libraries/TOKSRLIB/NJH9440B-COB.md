# NJH9440B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NJH9440B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　　　　　　　　　　        *
*    サブシステム　　　　：　ヨドバシ　ＥＤＩ　　　　　　　　　*
*    モジュール名　　　　：　発注変更データ反映ＳＵＢ　　　　　*
*    作成日／作成者　　　：　2021/07/30 NAV INOUE              *
*    処理概要　　　　　　：　パラメタを受け取り、基本情報Ｆ、　*
*                          　売上伝票Ｆの更新を行なう。　　　　*
*                          　また、在庫Ｍの更新も同時に行なう。*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2021/07/30 INOUE 　　新規作成
* 2022/02/14 TAKAHASHI 納品日更新方法条件変更
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NJH9440B.
*                  流用: SSY9308B.OKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/07/30.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 伝票データ >>--*
     SELECT   SHTDENLS  ASSIGN         DA-01-VI-SHTDENLS
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DEN-F01
                                       DEN-F111
                                       DEN-F02
                                       DEN-F03
                        WITH  DUPLICATES
                        FILE STATUS    DEN-ST.
*---<<  ヨドバシ基本情報ファイル  >>---*
     SELECT   YODJOHL1  ASSIGN    TO   DA-01-VI-YODJOHL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JOH-F17
                                       JOH-F03
                                       JOH-F02
                                       JOH-F07
                        WITH  DUPLICATES
                        FILE STATUS    JOH-ST.
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO   DA-01-VI-ZAMZAIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  ZAI-F01
                                       ZAI-F02
                                       ZAI-F03
                        FILE STATUS    ZAI-ST.
*---<<  商品名称マスタ  >>---*
     SELECT   MEIMS1    ASSIGN         DA-01-VI-MEIMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  MEI-F011
                                       MEI-F012
                        FILE STATUS    MEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  SHTDENLS           LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< ヨドバシ基本情報Ｆ >>--*
 FD  YODJOHL1           LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL1  OF        XFDLIB
              JOINING   JOH       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF            LABEL RECORD   IS   STANDARD.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  MEIMS1-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  YODJOHL1-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  SHTDENLS-INV-FLG    PIC  X(03)  VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST            PIC  X(02).
 01  JOH-ST            PIC  X(02).
 01  ZAI-ST            PIC  X(02).
 01  MEI-ST            PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
 01  WRK-NAME.
     03  WRK-KANA            PIC  X(15)        VALUE SPACE.
*
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  LINK-SUB-IN.
     03  LINK-SUB-BUMON    PIC X(04).
     03  LINK-SUB-TANCD    PIC X(02).
     03  LINK-SUB-BTDATE   PIC 9(08).
     03  LINK-SUB-BTTIME   PIC 9(04).
     03  LINK-SUB-BTTOKC   PIC 9(08).
*    03  LINK-SUB-SOKOCD   PIC X(02).
     03  LINK-SUB-DENNO    PIC 9(09).
     03  LINK-SUB-GYO      PIC 9(02).
     03  LINK-SUB-TENCD    PIC 9(05).
     03  LINK-SUB-TYPE     PIC X(06).
     03  LINK-SUB-CANCEL   PIC X(70).
     03  LINK-SUB-HACDT    PIC 9(08).
     03  LINK-SUB-HACNO    PIC 9(10).
     03  LINK-SUB-HACGYO   PIC 9(06).
*    03  LINK-SUB-IDCD     PIC X(10).
*    03  LINK-SUB-HINCD    PIC X(20).
*    03  LINK-SUB-CHGDT    PIC 9(08).
     03  LINK-SUB-TANKA    PIC 9(15).
     03  LINK-SUB-SURYO    PIC 9(15).
     03  LINK-SUB-NOUDT    PIC 9(08).
*    03  LINK-SUB-KONPO    PIC 9(07).
*    03  LINK-SUB-JUURYO   PIC 9(10).
*    03  LINK-SUB-SSCCCD   PIC X(48).
*    03  LINK-SUB-SURKBN   PIC X(01).
*    03  LINK-SUB-SURYO  PIC 9(10).
 01  LINK-SUB-OUT.
     03  LINK-SUB-PGDATE   PIC 9(08).
     03  LINK-SUB-PGTIME   PIC 9(06).
     03  LINK-SUB-KOUSIN1  PIC X(01).
     03  LINK-SUB-KOUSIN2  PIC X(01).
****************************************************************
 PROCEDURE              DIVISION  USING
                        LINK-SUB-IN  LINK-SUB-OUT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 SHTDENLS-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENLS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### NJH9440B SHTDENLS ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< ヨドバシ基本情報ファイル >>--*
 YODJOHL1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODJOHL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### NJH9440B YODJOHL1 ERROR " JOH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 在庫マスタ >>--*
 ZAMZAIF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZAMZAIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### NJH9440B ZAMZAIL1 ERROR " ZAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 MEIMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      MEIMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### NJH9440B MEIMS1 ERROR " MEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     ACCEPT   SYS-TIME       FROM TIME.
     ACCEPT   SYS-TIME2      FROM TIME.
*
*****DISPLAY "LINK-SUB-IN   = " LINK-SUB-IN  UPON CONS.
*****DISPLAY "LINK-SUB-OUT  = " LINK-SUB-OUT UPON CONS.
*
     OPEN     I-O       SHTDENLS  YODJOHL1  ZAMZAIF.
     OPEN     INPUT     MEIMS1.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*
     MOVE     SPACE               TO   LINK-SUB-KOUSIN1.
     MOVE     SPACE               TO   LINK-SUB-KOUSIN2.
*ヨドバシ基本情報Ｆ検索
     MOVE    LINK-SUB-BTTOKC      TO   JOH-F17.
     MOVE    LINK-SUB-HACDT       TO   JOH-F03.
     MOVE    LINK-SUB-HACNO       TO   JOH-F02.
     MOVE    LINK-SUB-HACGYO      TO   JOH-F07.
*T
*    DISPLAY "JOH-F17 = " JOH-F17  UPON CONS.
*    DISPLAY "JOH-F03 = " JOH-F03  UPON CONS.
*    DISPLAY "JOH-F02 = " JOH-F02  UPON CONS.
*    DISPLAY "JOH-F07 = " JOH-F07  UPON CONS.
*T
     PERFORM YODJOHL1-READ-SEC.
     IF      YODJOHL1-INV-FLG  =  "INV"
             MOVE    SPACE        TO   LINK-SUB-KOUSIN1
             DISPLAY NC"基本情報Ｆなし！？" UPON CONS
             GO                   TO   200-MAIN-RTN-EXIT
     ELSE
*          ヨドバシ基本情報Ｆ更新
*            メッセージタイプ
             MOVE    LINK-SUB-TYPE     TO   JOH-F01
*            キャンセル区分
             MOVE    LINK-SUB-CANCEL   TO   JOH-F09
*            明細単価
             IF    ( LINK-SUB-TANKA NOT =   JOH-F10 ) AND
                   ( LINK-SUB-TANKA NOT =   ZERO    )
                     MOVE LINK-SUB-TANKA    TO   JOH-F10
             END-IF
*            納入期日
             IF    ( LINK-SUB-NOUDT NOT =   JOH-F14 ) AND
*************2022/01/14 NAV ST
*******************( LINK-SUB-NOUDT NOT =   ZERO    )
                   ( LINK-SUB-NOUDT NOT =   ZERO    ) AND
                   ( LINK-SUB-NOUDT >       JOH-F14 )
*************2022/01/14 NAV ED
                     MOVE LINK-SUB-NOUDT    TO   JOH-F14
             END-IF
*            発注数量
             IF    ( LINK-SUB-SURYO NOT =   JOH-F13 )
                     MOVE LINK-SUB-SURYO    TO   JOH-F13
             END-IF
*            更新日
             MOVE    SYS-DATEW         TO   JOH-F24
*            更新時刻
             MOVE    SYS-TIMEW         TO   JOH-F25
*
             REWRITE  JOH-REC
             MOVE    "1"          TO   LINK-SUB-KOUSIN1
*****        DISPLAY "DDDDD" UPON CONS
     END-IF.
*
*売上伝票Ｆ検索
     MOVE    LINK-SUB-BTTOKC      TO   DEN-F01.
     MOVE    LINK-SUB-HACDT       TO   DEN-F111.
     MOVE    LINK-SUB-DENNO       TO   DEN-F02.
     MOVE    LINK-SUB-GYO         TO   DEN-F03.
     PERFORM SHTDENLS-READ-SEC.
     IF      SHTDENLS-INV-FLG  =  "INV"
             MOVE    SPACE        TO   LINK-SUB-KOUSIN2
             DISPLAY NC"売上伝票Ｆなし！？" UPON CONS
             DISPLAY NC"取引先ＣＤ＝"  LINK-SUB-BTTOKC UPON CONS
             DISPLAY NC"発注日　　＝"  LINK-SUB-HACDT  UPON CONS
             DISPLAY NC"伝票番号　＝"  LINK-SUB-DENNO  UPON CONS
             DISPLAY NC"行番号　　＝"  LINK-SUB-GYO    UPON CONS
     ELSE
*            在庫Ｍ更新
*            売上伝票Ｆ更新
***          IF  LINK-SUB-SURKBN  =  "1"
                 IF  LINK-SUB-SURYO  NOT =   DEN-F15
                      PERFORM ZAIKO-SEC
                      MOVE   DEN-F15             TO  DEN-F50
                      MOVE   LINK-SUB-SURYO      TO  DEN-F15
                      MOVE   LINK-SUB-TANKA      TO  DEN-F172
                      COMPUTE DEN-F181 = DEN-F15  *  DEN-F172
                      COMPUTE DEN-F182 = DEN-F15  *  DEN-F173
                 END-IF
***          END-IF
*************#2022/02/14 NAV ST
*************MOVE     LINK-SUB-NOUDT      TO   DEN-F112
             IF    ( LINK-SUB-NOUDT NOT =   DEN-F112 ) AND
                   ( LINK-SUB-NOUDT NOT =   ZERO     ) AND
                   ( LINK-SUB-NOUDT >       DEN-F112 )
                     MOVE LINK-SUB-NOUDT  TO   DEN-F112
             END-IF
*************#2022/02/14 NAV ED
             MOVE     9                   TO   DEN-F276
             REWRITE  DEN-REC
             MOVE    "1"                  TO   LINK-SUB-KOUSIN2
**********   DISPLAY "HHHHH" UPON CONS
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    SHTDENLS  YODJOHL1  ZAMZAIF  MEIMS1.
*
     MOVE     SYS-DATEW           TO       LINK-SUB-PGDATE.
     MOVE     SYS-TIMEW           TO       LINK-SUB-PGTIME.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヨドバシ基本情報Ｆ　読込
*--------------------------------------------------------------*
 YODJOHL1-READ-SEC       SECTION.
     MOVE    "YODJOHL1-READ-SEC"   TO   S-NAME.
*
     READ  YODJOHL1      INVALID
                        MOVE "INV" TO  YODJOHL1-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  YODJOHL1-INV-FLG
     END-READ.
*
 YODJOHL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    売上伝票Ｆ　　　　読込
*--------------------------------------------------------------*
 SHTDENLS-READ-SEC      SECTION.
     MOVE    "SHTDENLS-READ-SEC"  TO   S-NAME.
*
     READ  SHTDENLS     INVALID
                        MOVE "INV" TO  SHTDENLS-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  SHTDENLS-INV-FLG
     END-READ.
*
 SHTDENL1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品名称Ｍ　　　　読込
*--------------------------------------------------------------*
 MEIMS1-READ-SEC        SECTION.
     MOVE    "MEIMS1-READ-SEC"    TO   S-NAME.
*
     READ  MEIMS1       INVALID
                        MOVE "INV" TO  MEIMS1-INV-FLG
                        MOVE SPACE TO  WRK-KANA
                   NOT  INVALID
                        MOVE SPACE TO  MEIMS1-INV-FLG
                        MOVE MEI-F031  TO  WRK-KANA
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE   "ZAIKO-SEC"      TO   S-NAME.
     MOVE    SPACE           TO   KEP-FLG.
*商品在庫マスタ存在チェック
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021  MEI-F011.
     MOVE    DEN-F1412       TO   ZAI-F022  MEI-F012.
     MOVE    DEN-F49         TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID
             PERFORM   MEIMS1-READ-SEC
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫Ｍが未存在の為、在庫マスタ作成
     MOVE      "1"           TO   KEP-FLG.
     IF        MEIMS1-INV-FLG      =   "INV"
               GO  TO   ZAIKO-UPDATE1-EXIT
     END-IF.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      DEN-F08       TO   ZAI-F01.
      MOVE      DEN-F1411     TO   ZAI-F021.
      MOVE      DEN-F1412     TO   ZAI-F022.
      MOVE      DEN-F49       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F27       =    ZAI-F27  +  LINK-SUB-SURYO
*取引先
      MOVE      DEN-F01       TO   ZAI-F29.
*カナ名称
      MOVE      WRK-KANA      TO   ZAI-F30.
      MOVE      SYS-DATEW     TO   ZAI-F98.
      MOVE      SYS-DATEW     TO   ZAI-F99.
      WRITE     ZAI-REC.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*              在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
     INITIALIZE    WRK-AREA2.
*
     IF  DEN-F27D      =     1
*        引当済数に数量減算
         COMPUTE  ZAI-F28   =   ZAI-F28  -  DEN-F15
*        未出庫数に数量減算
         COMPUTE  ZAI-F27   =   ZAI-F27  -  DEN-F15
     ELSE
*        未出庫数に数量減算
         COMPUTE  ZAI-F27   =   ZAI-F27  -  DEN-F15
     END-IF.
*
*引当後在庫数チェック
*    現在庫数－引当済数＝引当可能在庫数
     COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  LINK-SUB-SURYO.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  LINK-SUB-SURYO
         MOVE     SYS-DATEW       TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
*        引当済数に数量加算
         COMPUTE  ZAI-F28   =   ZAI-F28  +  LINK-SUB-SURYO
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  LINK-SUB-SURYO
         MOVE     SYS-DATEW       TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
