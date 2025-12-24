# STN0190L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/STN0190L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ営業第２部　　　　　　*
*    業務名　　　　　　　：　ＨＨＴ_卸業務　                  *
*    モジュール名　　　　：　_卸初期処理確認リスト            *
*    作成日／更新日　　　：　2021/03/15                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　_卸初期処理の確認リストを出力する*
*                        ：　                                  *
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            STN0190L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          01/05/10.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<< 条件ファイル    >>----*
     SELECT   JYOKEN    ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
*
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 条件ファイル    >>----*
 FD  JYOKEN.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<<プリントファイル>>----*
 FD  PRTFILE
     LABEL    RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER         PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  X(03)   VALUE SPACE.
     03  ERR-FLG1       PIC  X(03)   VALUE SPACE.
     03  ERR-FLG2       PIC  X(03)   VALUE SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  JYO-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "STN0190L".
 01  TANAOROSI-BI      PIC  9(08)      VALUE  ZERO.
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  HD1-00                  PIC  X(08).
     03  FILLER                  PIC  X(30)  VALUE  SPACE.
     03  FILLER                  PIC  N(13)  VALUE
         NC"※※_卸初期確認リスト※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(20)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2.
     03  FILLER                  PIC  X(30)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"_卸日"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(13)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"原票_開始"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(09)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"原票_終了"
                                 CHARACTER   TYPE  IS  CHR-2.
*
 01  SEN0                        CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(136) VALUE ALL "-".
*
 01  DT1.
     03  FILLER                  PIC  X(30)  VALUE  SPACE.
     03  DT1-00                  PIC  X(10).
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  DT1-01                  PIC  ZZZZZZZZ9.
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  DT1-02                  PIC  ZZZZZZZZ9.
*
 01  DT2                         CHARACTER   TYPE   IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(24)  VALUE
     NC"＊＊＊　条件マスタに登録されていません。　＊＊＊".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 JYOERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### STN0190L JYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** STN0190L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     JYOKEN.
     OPEN     OUTPUT    PRTFILE.
*ヘッダ行印字
     PERFORM       HEAD-WT-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
*****原票ＮＯ取得
     MOVE     SPACE     TO   JYO-REC.
     INITIALIZE              JYO-REC.
     MOVE    "63"       TO   JYO-F01.
     MOVE     SPACE     TO   JYO-F02.
     PERFORM JYO-RD-SEC.
     MOVE    ERR-FLG    TO   ERR-FLG1.
     IF   ERR-FLG1 = "OK"
          MOVE     JYO-F04   TO     DT1-01
          MOVE     JYO-F05   TO     DT1-02
     ELSE
          DISPLAY NC"＃＃原票ＮＯ取得エラー＃＃" UPON CONS
          WRITE    PRT-REC   FROM   DT2    AFTER  2
          MOVE     4000      TO     PROGRAM-STATUS
          GO                 TO     200-MAIN-RTN-EXIT
     END-IF.
*****_卸日取得
     MOVE     SPACE     TO   JYO-REC.
     INITIALIZE              JYO-REC.
     MOVE    "99"       TO   JYO-F01.
     MOVE    "TANA"     TO   JYO-F02.
     PERFORM JYO-RD-SEC.
     MOVE    ERR-FLG    TO   ERR-FLG2.
     IF   ERR-FLG2 = "OK"
          MOVE     JYO-F04   TO     TANAOROSI-BI
          MOVE     TANAOROSI-BI(1:4)  TO  DT1-00(1:4)
          MOVE     "/"                TO  DT1-00(5:1)
          MOVE     TANAOROSI-BI(5:2)  TO  DT1-00(6:2)
          MOVE     "/"                TO  DT1-00(8:1)
          MOVE     TANAOROSI-BI(7:2)  TO  DT1-00(9:2)
     ELSE
          DISPLAY NC"＃＃_卸日取得エラー　＃＃" UPON CONS
          WRITE    PRT-REC   FROM   DT2    AFTER  2
          MOVE     4000      TO     PROGRAM-STATUS
          GO                 TO     200-MAIN-RTN-EXIT
     END-IF.
*    明細取得
     WRITE    PRT-REC   FROM   DT1    AFTER  2  .
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    JYOKEN    PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** STN0190L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  条件ファイル（原票_）                      *
*--------------------------------------------------------------*
 JYO-RD-SEC             SECTION.
     READ     JYOKEN
         INVALID   KEY
              MOVE     "ERR"      TO   ERR-FLG
         NOT INVALID KEY
              MOVE     "OK "      TO   ERR-FLG
     END-READ.
 JYO-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
***  日付
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     SYS-DATE            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD(1:4)   TO        HD1-01.
     MOVE     LINK-OUT-YMD(5:2)   TO        HD1-02.
     MOVE     LINK-OUT-YMD(7:2)   TO        HD1-03.
***  ページ_
     MOVE     1                   TO        HD1-04.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   SEN0      AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN0      AFTER  1.
 HEAD-WT-EXIT.
     EXIT.

```
