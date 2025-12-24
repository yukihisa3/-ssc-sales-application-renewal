# SSY9420R

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9420R.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　　        *
*    サブシステム　　　　：　ヨドバシ　　　　　　　　　　　　　*
*    モジュール名　　　　：　ヨドバシ納期一括変更更新（サブ）　*
*    作成日／作成者　　　：　2022/09/15 NAV INOUE              *
*    処理概要　　　　　　：　売上伝票ファイル・基本情報ファイル*
*                          　の納品日（納入期日）を一括変更　　*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2022/09/07 TAKAHASHI新規作成  流用：SSY9420Bからリカバリ用
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY9420R.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/02/15.
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
     SELECT   SHTDENLJ  ASSIGN         DA-01-VI-SHTDENLJ
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F112
                                       DEN-F48   DEN-F02
                                       DEN-F04   DEN-F051
                                       DEN-F07   DEN-F03
                        WITH  DUPLICATES
                        FILE STATUS    DEN-ST.
*---<<  基本情報ファイル　　　  >>---*
     SELECT   YODJOHL2  ASSIGN    TO   DA-01-VI-YODJOHL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JOH-F15  JOH-F16
                                       JOH-F17  JOH-F03
                                       JOH-F18  JOH-F19
                        WITH  DUPLICATES
                        FILE STATUS    JOH-ST.
*---<<  納期変更・出荷確定Ｆ　  >>---*
     SELECT   YODNKKL1  ASSIGN    TO   DA-01-VI-YODNKKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE DYNAMIC
                        RECORD    KEY  NKK-F01  NKK-F02
                                       NKK-F03  NKK-F04
                                       NKK-F05  NKK-F06
                                       NKK-F07  NKK-F08
                                       NKK-F09  NKK-F10
                                       NKK-F11
                        WITH  DUPLICATES
                        FILE STATUS    NKK-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  SHTDENLJ           LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 基本情報ファイル >>--*
 FD  YODJOHL2           LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL2  OF        XFDLIB
              JOINING   JOH       PREFIX.
*----<< 納期変更・出荷確定Ｆ >>--*
 FD  YODNKKL1           LABEL RECORD   IS   STANDARD.
     COPY     YODNKKL1  OF        XFDLIB
              JOINING   NKK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  SHTDENLJ-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  SHTDENLJ-END-FLG    PIC  X(03)  VALUE  SPACE.
     03  YODJOHL2-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  YODJOHL2-END-FLG    PIC  X(03)  VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST            PIC  X(02)        VALUE  SPACE.
 01  JOH-ST            PIC  X(02)        VALUE  SPACE.
 01  NKK-ST            PIC  X(02)        VALUE  SPACE.
*
*----<< CNT        >>--*
 01  DEN-CNT           PIC  9(07)        VALUE  ZERO.
 01  JOH-CNT           PIC  9(07)        VALUE  ZERO.
 01  NKK-CNT           PIC  9(07)        VALUE  ZERO.
*----<< WORK       >>--*
 01  WK-DEN.
     03 WK-DENNO       PIC  9(09)        OCCURS 20.
 01  IX                PIC  9(02)        VALUE  ZERO.
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
*01  SYS-TIME2          PIC  9(08).
*01  FILLER             REDEFINES      SYS-TIME2.
 01  FILLER             REDEFINES      SYS-TIME.
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
 LINKAGE                    SECTION.
 01  LINK-IN-DATE           PIC 9(08).
 01  LINK-IN-TIME           PIC 9(04).
 01  LINK-IN-TORICD         PIC 9(08).
 01  LINK-IN-SOKCD          PIC X(02).
 01  LINK-IN-FDATE          PIC 9(08).
 01  LINK-IN-TDATE          PIC 9(08).
 01  LINK-IN-DENNO          PIC 9(09).
 01  LINK-IN-GYO            PIC 9(09).
 01  LINK-IN-TENCD          PIC 9(09).
 01  LINK-IN-TANCD          PIC X(02).
*
****************************************************************
 PROCEDURE              DIVISION  USING
                                  LINK-IN-DATE
                                  LINK-IN-TIME
                                  LINK-IN-TORICD
                                  LINK-IN-SOKCD
                                  LINK-IN-FDATE
                                  LINK-IN-TDATE
                                  LINK-IN-DENNO
                                  LINK-IN-GYO
                                  LINK-IN-TENCD
                                  LINK-IN-TANCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 SHTDENLJ-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENLJ.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9420R SHTDENLJ ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 基本情報ファイル　　　 >>--*
 YODJOHL2-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODJOHL2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9420R YODJOHL2 ERROR " JOH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 納期変更・出荷確定Ｆ　 >>--*
 YODNKKL1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      YODNKKL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY9420R YODNKKL1 ERROR " NKK-ST " "
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
     PERFORM  201-MAIN-RTN.
     PERFORM  300-END-RTN.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*
 100-INIT-01.
     MOVE     LINK-IN-DENNO       TO   WK-DEN.
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
*
*T↓
*****DISPLAY "LINK-IN-TANCD = " LINK-IN-TANCD  UPON CONS.
*****DISPLAY "LINK-IN-DATE  = " LINK-IN-DATE   UPON CONS.
*****DISPLAY "LINK-TIME     = " LINK-TIME      UPON CONS.
*****DISPLAY "LINK-TORICD   = " LINK-TORICD    UPON CONS.
*****DISPLAY "LINK-SOKCD    = " LINK-SOKCD     UPON CONS.
*****DISPLAY "LINK-FDATE    = " LINK-FDATE     UPON CONS.
*****DISPLAY "LINK-TDATE    = " LINK-TDATE     UPON CONS.
*****DISPLAY "LINK-DENNO    = " LINK-DENNO     UPON CONS.
*T↑
*
 100-INIT-02.
     OPEN     I-O       SHTDENLJ  YODJOHL2  YODNKKL1.
*
 100-INIT-03.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ  伝票番号指定あり                   *
*--------------------------------------------------------------*
 201-MAIN-RTN           SECTION.
     MOVE    "201-MAIN-RTN"       TO   S-NAME.
*
 201-MAIN-00.
 201-MAIN-01.
 201-MAIN-02.
*　　売上伝票ファイルＳＴＡＲＴ
     MOVE    SPACE              TO     DEN-REC.
     INITIALIZE                        DEN-REC.
     MOVE    LINK-IN-DATE       TO     DEN-F46.
     MOVE    LINK-IN-TIME       TO     DEN-F47.
     MOVE    LINK-IN-TORICD     TO     DEN-F01.
     MOVE    LINK-IN-FDATE      TO     DEN-F112.
     MOVE    LINK-IN-SOKCD      TO     DEN-F48.
     MOVE    LINK-IN-DENNO      TO     DEN-F02.
     MOVE    0                  TO     DEN-F04.
     MOVE    40                 TO     DEN-F051.
     MOVE    LINK-IN-TENCD      TO     DEN-F07.
     MOVE    LINK-IN-GYO        TO     DEN-F03.
*　　売上伝票ファイルＲＥＡＤ
     PERFORM SHTDENLJ-READ2-SEC.
     IF      SHTDENLJ-INV-FLG    =     "INV"
             DISPLAY NC"売上伝票ファイル　対象なし" UPON CONS
             DISPLAY "DEN-NO = " DEN-F02            UPON CONS
             GO                 TO     201-MAIN-EXIT
     END-IF.
*
*基本情報ファイル検索
 201-MAIN-04.
     MOVE    DEN-F46              TO   JOH-F15.
     MOVE    DEN-F47              TO   JOH-F16.
     MOVE    DEN-F01              TO   JOH-F17.
     MOVE    DEN-F111             TO   JOH-F03.
     MOVE    DEN-F02              TO   JOH-F18.
     MOVE    DEN-F03              TO   JOH-F19.
     PERFORM YODJOHL2-READ-SEC.
*
*売上伝票ファイル更新
*　ここにはＨＩＴしかこない
 201-MAIN-05.
     IF      SHTDENLJ-INV-FLG NOT =  "INV"
             MOVE     LINK-IN-TDATE   TO   DEN-F112
             MOVE     LINK-IN-TANCD   TO   DEN-F60
             MOVE     SYS-DATEW       TO   DEN-F63
             MOVE     SYS-TIMEW       TO   DEN-F67
             REWRITE  DEN-REC
             ADD      1               TO   DEN-CNT
     END-IF.
*
*基本情報ファイル更新
 201-MAIN-06.
     IF      YODJOHL2-END-FLG =  "HIT"
             MOVE     LINK-IN-TDATE   TO   JOH-F14
             MOVE     SYS-DATEW       TO   JOH-F24
             MOVE     SYS-TIMEW       TO   JOH-F25
             REWRITE  JOH-REC
             ADD      1               TO   JOH-CNT
     END-IF.
*
*納期変更・出荷確定Ｆ出力
     MOVE     SPACE               TO   NKK-REC.
     INITIALIZE                        NKK-REC.
     IF      YODJOHL2-END-FLG =  "END"
             MOVE     "1"         TO   NKK-F50
             MOVE     "1"         TO   NKK-F53
     ELSE
             MOVE     "1"         TO   NKK-F60
     END-IF.
     PERFORM YODNKKL1-WRITE-SEC.
*
*
 201-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    SHTDENLJ  YODJOHL2  YODNKKL1.
*****DISPLAY  NC"売上伝票ファイル更新＝" DEN-CNT NC"件"
*                                                    UPON CONS.
*    DISPLAY  NC"基本情報ファイル更新＝" JOH-CNT NC"件"
*                                                    UPON CONS.
*    DISPLAY  NC"納期変更・出荷確定Ｆ＝" NKK-CNT NC"件"
*****                                                UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    売上伝票ファイルＲＥＡＤ（伝票番号指定あり）
*--------------------------------------------------------------*
 SHTDENLJ-READ2-SEC      SECTION.
     MOVE  "SHTDENLJ-READ2-SEC"     TO  S-NAME.
*
 SHTDENLJ-READ2-01.
     READ   SHTDENLJ
               INVALID
                        MOVE "INV" TO  SHTDENLJ-INV-FLG
                        GO         TO  SHTDENLJ-READ2-EXIT
               NOT INVALID
                        MOVE "   " TO  SHTDENLJ-INV-FLG
     END-READ.
*
     IF        DEN-F277     =    9
               MOVE     SPACE         TO   NKK-REC
               INITIALIZE                  NKK-REC
               MOVE     "1"           TO   NKK-F50
               MOVE     "1"           TO   NKK-F51
               PERFORM  YODNKKL1-WRITE-SEC
               MOVE "INV" TO  SHTDENLJ-INV-FLG
               GO         TO  SHTDENLJ-READ2-EXIT
     END-IF.
*
 SHTDENLJ-READ2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    基本情報ファイル　ＲＥＡＤ
*--------------------------------------------------------------*
 YODJOHL2-READ-SEC      SECTION.
     MOVE  "YODJOHL2-READ-SEC"     TO  S-NAME.
*
 YODJOHL2-READ-01.
     READ   YODJOHL2
               INVALID
                        MOVE "END" TO  YODJOHL2-END-FLG
               NOT INVALID
                        MOVE "HIT" TO  YODJOHL2-END-FLG
     END-READ.
*
 YODJOHL2-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    納期変更・出荷確定Ｆ　ＷＲＩＴＥ
*--------------------------------------------------------------*
 YODNKKL1-WRITE-SEC    SECTION.
     MOVE  "YODNKKL1-WRITE-SEC"    TO  S-NAME.
*
 YODNKKL1-WRITE-SEC-01.
     MOVE   "1"                    TO  NKK-F01.
     MOVE   SYS-DATEW              TO  NKK-F02.
     MOVE   SYS-TIMEW              TO  NKK-F03.
     MOVE   LINK-IN-DATE           TO  NKK-F04.
     MOVE   LINK-IN-TIME           TO  NKK-F05.
     MOVE   LINK-IN-TORICD         TO  NKK-F06.
     MOVE   LINK-IN-SOKCD          TO  NKK-F07.
     MOVE   LINK-IN-FDATE          TO  NKK-F08.
     MOVE   LINK-IN-TENCD          TO  NKK-F09.
     MOVE   LINK-IN-DENNO          TO  NKK-F10.
     MOVE   LINK-IN-GYO            TO  NKK-F11.
     MOVE   LINK-IN-TDATE          TO  NKK-F12.
     MOVE   LINK-IN-TANCD          TO  NKK-F61.
*
 YODNKKL1-WRITE-SEC-02.
     WRITE  NKK-REC.
     ADD    1                      TO  NKK-CNT.
*
 YODNKKL1-WRITE-SEC-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
