# SSY9450B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9450B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　　        *
*    サブシステム　　　　：　ヨドバシ　　　　　　　　　　　　　*
*    モジュール名　　　　：　ヨドバシ出荷確定更新　　　　　　　*
*    作成日／作成者　　　：　2022/02/18 NAV INOUE              *
*    処理概要　　　　　　：　売上伝票ファイル付番を一括変更　　*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2022/02/18 INOUE 　　新規作成  流用：SSY9420B.TOKSRLIB
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY9450B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/02/18.
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
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F112
                                       DEN-F48   DEN-F02
                                       DEN-F04   DEN-F051
                                       DEN-F07   DEN-F03
                        WITH  DUPLICATES
                        FILE STATUS    DEN-ST.
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
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST            PIC  X(02)        VALUE  SPACE.
 01  NKK-ST            PIC  X(02)        VALUE  SPACE.
*
*----<< CNT        >>--*
 01  DEN-CNT           PIC  9(07)        VALUE  ZERO.
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
 01  LINK-IN-NDATE          PIC 9(08).
 01  LINK-IN-DENKBN         PIC X(01).
 01  LINK-IN-DENNO.
     03  LINK-IN-DENNO-01   PIC  9(09).
     03  LINK-IN-DENNO-02   PIC  9(09).
     03  LINK-IN-DENNO-03   PIC  9(09).
     03  LINK-IN-DENNO-04   PIC  9(09).
     03  LINK-IN-DENNO-05   PIC  9(09).
     03  LINK-IN-DENNO-06   PIC  9(09).
     03  LINK-IN-DENNO-07   PIC  9(09).
     03  LINK-IN-DENNO-08   PIC  9(09).
     03  LINK-IN-DENNO-09   PIC  9(09).
     03  LINK-IN-DENNO-10   PIC  9(09).
     03  LINK-IN-DENNO-11   PIC  9(09).
     03  LINK-IN-DENNO-12   PIC  9(09).
     03  LINK-IN-DENNO-13   PIC  9(09).
     03  LINK-IN-DENNO-14   PIC  9(09).
     03  LINK-IN-DENNO-15   PIC  9(09).
     03  LINK-IN-DENNO-16   PIC  9(09).
     03  LINK-IN-DENNO-17   PIC  9(09).
     03  LINK-IN-DENNO-18   PIC  9(09).
     03  LINK-IN-DENNO-19   PIC  9(09).
     03  LINK-IN-DENNO-20   PIC  9(09).
 01  LINK-IN-TANCD          PIC  X(02).
 01  LINK-OUT-JDATE         PIC  9(08).
 01  LINK-OUT-JTIME         PIC  9(06).
*
****************************************************************
 PROCEDURE              DIVISION  USING
                                  LINK-IN-DATE
                                  LINK-IN-TIME
                                  LINK-IN-TORICD
                                  LINK-IN-SOKCD
                                  LINK-IN-NDATE
                                  LINK-IN-DENKBN
                                  LINK-IN-DENNO
                                  LINK-IN-TANCD
                                  LINK-OUT-JDATE
                                  LINK-OUT-JTIME.
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
     DISPLAY  "### SSY9450B SHTDENLJ ERROR " DEN-ST " "
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
     DISPLAY  "### SSY9450B YODNKKL1 ERROR " NKK-ST " "
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
     IF       LINK-IN-DENKBN =    " "
              PERFORM  200-MAIN-RTN
     ELSE
              PERFORM  201-MAIN-RTN
     END-IF.
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
*****DISPLAY "LINK-NDATE    = " LINK-NDATE     UPON CONS.
*****DISPLAY "LINK-DENKBN   = " LINK-DENKBN    UPON CONS.
*****DISPLAY "LINK-DENNO    = " LINK-DENNO     UPON CONS.
*T↑
*
 100-INIT-02.
     OPEN     I-O       SHTDENLJ  YODNKKL1.
*
 100-INIT-03.
*　　売上伝票ファイルＳＴＡＲＴ
     MOVE    SPACE              TO     DEN-REC.
     INITIALIZE                        DEN-REC.
     MOVE    LINK-IN-DATE       TO     DEN-F46.
     MOVE    LINK-IN-TIME       TO     DEN-F47.
     MOVE    LINK-IN-TORICD     TO     DEN-F01.
     MOVE    LINK-IN-NDATE      TO     DEN-F112.
     MOVE    LINK-IN-SOKCD      TO     DEN-F48.
     PERFORM SHTDENLJ-START-SEC.
     IF      SHTDENLJ-INV-FLG    =     "INV"
             MOVE    "END"      TO     SHTDENLJ-END-FLG
             DISPLAY NC"売上伝票ファイル　対象なし" UPON CONS
             GO                 TO     100-INIT-RTN-EXIT
     END-IF.
 100-INIT-04.
*　　売上伝票ファイルＲＥＡＤ
     PERFORM SHTDENLJ-READ1-SEC.
     IF      SHTDENLJ-END-FLG    =     "END"
             DISPLAY NC"売上伝票ファイル　対象なし" UPON CONS
             GO                 TO     100-INIT-RTN-EXIT
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ  伝票番号指定なし                   *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*
*終了判定　　　　　　
 200-MAIN-01.
     IF      SHTDENLJ-END-FLG =  "END"
             GO                   TO   200-MAIN-EXIT
     END-IF.
*
*売上伝票ファイル更新
*　ここにはＨＩＴしかこない
 200-MAIN-03.
     IF      SHTDENLJ-END-FLG =  "HIT"
             MOVE     9               TO   DEN-F276
             MOVE     LINK-IN-TANCD   TO   DEN-F60
             MOVE     SYS-DATEW       TO   DEN-F63
             MOVE     SYS-TIMEW       TO   DEN-F67
             REWRITE  DEN-REC
             ADD      1               TO   DEN-CNT
     END-IF.
*
*納期変更・出荷確定Ｆ出力
 200-MAIN-05.
     MOVE     SPACE               TO   NKK-REC.
     INITIALIZE                        NKK-REC.
     MOVE    "1"                  TO   NKK-F60.
     PERFORM  YODNKKL1-WRITE-SEC.
*
*売上伝票ファイルＲＥＡＤ
 200-MAIN-06.
     PERFORM  SHTDENLJ-READ1-SEC.
     GO                           TO   200-MAIN-01.
*
 200-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ  伝票番号指定あり                   *
*--------------------------------------------------------------*
 201-MAIN-RTN           SECTION.
     MOVE    "201-MAIN-RTN"       TO   S-NAME.
*
 201-MAIN-00.
     MOVE    0                  TO     IX.
 201-MAIN-01.
     ADD     1                  TO     IX.
*    終了判定　　　　　　
     IF    ( IX  >  20 ) OR
           ( WK-DENNO(IX) = "000000000" )
             GO                 TO     201-MAIN-EXIT
     END-IF.
 201-MAIN-02.
*　　売上伝票ファイルＳＴＡＲＴ
     MOVE    SPACE              TO     DEN-REC.
     INITIALIZE                        DEN-REC.
     MOVE    LINK-IN-DATE       TO     DEN-F46.
     MOVE    LINK-IN-TIME       TO     DEN-F47.
     MOVE    LINK-IN-TORICD     TO     DEN-F01.
     MOVE    LINK-IN-NDATE      TO     DEN-F112.
     MOVE    LINK-IN-SOKCD      TO     DEN-F48.
     MOVE    WK-DENNO(IX)       TO     DEN-F02.
     PERFORM SHTDENLJ-START-SEC.
     IF      SHTDENLJ-INV-FLG    =     "INV"
*            DISPLAY NC"売上伝票ファイル　対象なし" UPON CONS
             MOVE     SPACE           TO   NKK-REC
             INITIALIZE                    NKK-REC
             MOVE     "1"             TO   NKK-F50
             MOVE     "1"             TO   NKK-F52
             PERFORM  YODNKKL1-WRITE-SEC
             GO                 TO     201-MAIN-01
     END-IF.
 201-MAIN-03.
*　　売上伝票ファイルＲＥＡＤ
     PERFORM SHTDENLJ-READ2-SEC.
     IF      SHTDENLJ-END-FLG    =     "END"
*            DISPLAY NC"売上伝票ファイル　対象なし" UPON CONS
             MOVE     SPACE           TO   NKK-REC
             INITIALIZE                    NKK-REC
             MOVE     "1"             TO   NKK-F50
             MOVE     "1"             TO   NKK-F52
             PERFORM  YODNKKL1-WRITE-SEC
             GO                 TO     201-MAIN-01
     END-IF.
     IF      SHTDENLJ-END-FLG    =     "SKP"
             GO                 TO     201-MAIN-01
     END-IF.
*
*売上伝票ファイル更新
*　ここにはＨＩＴしかこない
 201-MAIN-05.
     IF      SHTDENLJ-END-FLG =  "HIT"
             MOVE     9               TO   DEN-F276
             MOVE     LINK-IN-TANCD   TO   DEN-F60
             MOVE     SYS-DATEW       TO   DEN-F63
             MOVE     SYS-TIMEW       TO   DEN-F67
             REWRITE  DEN-REC
             ADD      1               TO   DEN-CNT
     END-IF.
*
*納期変更・出荷確定Ｆ出力
     MOVE     SPACE               TO   NKK-REC.
     INITIALIZE                        NKK-REC.
     MOVE    "1"                  TO   NKK-F60.
     PERFORM YODNKKL1-WRITE-SEC.
*
     GO                   TO   201-MAIN-01.
*
 201-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    SHTDENLJ  YODNKKL1.
     DISPLAY  NC"売上伝票ファイル更新＝" DEN-CNT NC"件"
                                                     UPON CONS.
     DISPLAY  NC"納期変更・出荷確定Ｆ＝" NKK-CNT NC"件"
                                                     UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    売上伝票ファイルＳＴＡＲＴ
*--------------------------------------------------------------*
 SHTDENLJ-START-SEC      SECTION.
     MOVE    "SHTDENLJ-START-SEC"  TO   S-NAME.
*
     START   SHTDENLJ  KEY  IS  >=     DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F112
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F03
             INVALID
                       MOVE   "INV"    TO   SHTDENLJ-INV-FLG
             NOT INVALID
                       MOVE   "   "    TO   SHTDENLJ-INV-FLG
     END-START.
*
 SHTDENLJ-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    売上伝票ファイルＲＥＡＤ（伝票番号指定なし）
*--------------------------------------------------------------*
 SHTDENLJ-READ1-SEC      SECTION.
     MOVE  "SHTDENLJ-READ1-SEC"    TO  S-NAME.
*
 SHTDENLJ-READ1-01.
     READ   SHTDENLJ
               AT  END
                        MOVE "END" TO  SHTDENLJ-END-FLG
                        GO         TO  SHTDENLJ-READ1-EXIT
               NOT AT END
                        MOVE "   " TO  SHTDENLJ-END-FLG
     END-READ.
*
*範囲チェック
 SHTDENLJ-READ1-02.
     IF   ( DEN-F46   =   LINK-IN-DATE )   AND
          ( DEN-F47   =   LINK-IN-TIME )   AND
          ( DEN-F01   =   LINK-IN-TORICD )
            CONTINUE
     ELSE
            MOVE    "END"          TO  SHTDENLJ-END-FLG
            GO                     TO  SHTDENLJ-READ1-EXIT
     END-IF.
*
*納品日一致チェック
 SHTDENLJ-READ1-03.
     IF     DEN-F112       =   LINK-IN-NDATE
            CONTINUE
     ELSE
            MOVE    "END"          TO  SHTDENLJ-END-FLG
            GO                     TO  SHTDENLJ-READ1-EXIT
     END-IF.
*
*倉庫一致チェック
 SHTDENLJ-READ1-04.
     IF     LINK-IN-SOKCD  =  SPACE
            CONTINUE
     ELSE
            IF   DEN-F48   =   LINK-IN-SOKCD
                 CONTINUE
            ELSE
                 MOVE    "END"          TO  SHTDENLJ-END-FLG
                 GO                     TO  SHTDENLJ-READ1-EXIT
            END-IF
     END-IF.
*
*計上済みチェック
 SHTDENLJ-READ1-05.
     IF   LINK-IN-DENKBN   NOT =  " "
          GO               TO               SHTDENLJ-READ1-EXIT
     END-IF.
*
     MOVE "   "            TO               SHTDENLJ-END-FLG.
     IF   DEN-F277         =  9
          MOVE     SPACE           TO   NKK-REC
          INITIALIZE                    NKK-REC
          MOVE     "1"             TO   NKK-F50
          MOVE     "1"             TO   NKK-F51
          PERFORM  YODNKKL1-WRITE-SEC
     ELSE
          MOVE     "HIT"           TO   SHTDENLJ-END-FLG
     END-IF.
*
     IF   SHTDENLJ-END-FLG  =  "HIT"
          GO            TO      SHTDENLJ-READ1-EXIT
     ELSE
          GO            TO      SHTDENLJ-READ1-01
     END-IF.
*
 SHTDENLJ-READ1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    売上伝票ファイルＲＥＡＤ（伝票番号指定あり）
*--------------------------------------------------------------*
 SHTDENLJ-READ2-SEC      SECTION.
     MOVE  "SHTDENLJ-READ2-SEC"     TO  S-NAME.
*
 SHTDENLJ-READ2-01.
     READ   SHTDENLJ
               AT  END
                        MOVE "END" TO  SHTDENLJ-END-FLG
                        GO         TO  SHTDENLJ-READ2-EXIT
               NOT AT END
                        MOVE "   " TO  SHTDENLJ-END-FLG
     END-READ.
*
*範囲チェック
 SHTDENLJ-READ2-02.
     IF   ( DEN-F46   =   LINK-IN-DATE )   AND
          ( DEN-F47   =   LINK-IN-TIME )   AND
          ( DEN-F01   =   LINK-IN-TORICD )
            CONTINUE
     ELSE
            MOVE    "END"          TO  SHTDENLJ-END-FLG
            GO                     TO  SHTDENLJ-READ2-EXIT
     END-IF.
*
*納品日一致チェック
 SHTDENLJ-READ2-03.
     IF     DEN-F112       =   LINK-IN-NDATE
            CONTINUE
     ELSE
            MOVE    "END"          TO  SHTDENLJ-END-FLG
            GO                     TO  SHTDENLJ-READ2-EXIT
     END-IF.
*
*倉庫一致チェック
 SHTDENLJ-READ2-04.
     IF     LINK-IN-SOKCD  =  SPACE
            CONTINUE
     ELSE
            IF   DEN-F48   =   LINK-IN-SOKCD
                 CONTINUE
            ELSE
                 MOVE    "END"          TO  SHTDENLJ-END-FLG
                 GO                     TO  SHTDENLJ-READ2-EXIT
            END-IF
     END-IF.
*
*伝票番号一致チェック
 SHTDENLJ-READ2-05.
     IF        DEN-F02  NOT =    WK-DENNO(IX)
               MOVE    "END"          TO   SHTDENLJ-END-FLG
               GO                     TO   SHTDENLJ-READ2-EXIT
     END-IF.
     IF        DEN-F277     =    9
               MOVE     SPACE         TO   NKK-REC
               INITIALIZE                  NKK-REC
               MOVE     "1"           TO   NKK-F50
               MOVE     "1"           TO   NKK-F51
               PERFORM  YODNKKL1-WRITE-SEC
               MOVE    "SKP"          TO   SHTDENLJ-END-FLG
     ELSE
               MOVE    "HIT"          TO   SHTDENLJ-END-FLG
     END-IF.
*
 SHTDENLJ-READ2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    納期変更・出荷確定Ｆ　ＷＲＩＴＥ
*--------------------------------------------------------------*
 YODNKKL1-WRITE-SEC    SECTION.
     MOVE  "YODNKKL1-WRITE-SEC"    TO  S-NAME.
*
 YODNKKL1-WRITE-SEC-01.
     MOVE   "2"                    TO  NKK-F01.
     MOVE   SYS-DATEW              TO  NKK-F02 LINK-OUT-JDATE.
     MOVE   SYS-TIMEW              TO  NKK-F03 LINK-OUT-JTIME.
     MOVE   LINK-IN-DATE           TO  NKK-F04.
     MOVE   LINK-IN-TIME           TO  NKK-F05.
     MOVE   LINK-IN-TORICD         TO  NKK-F06.
     MOVE   LINK-IN-SOKCD          TO  NKK-F07.
     MOVE   LINK-IN-NDATE          TO  NKK-F08.
     IF     LINK-IN-DENKBN =  " "
            MOVE DEN-F07           TO  NKK-F09
     ELSE
            IF   SHTDENLJ-END-FLG  =  "HIT"
                 MOVE  DEN-F07     TO  NKK-F09
            END-IF
     END-IF.
     IF     LINK-IN-DENKBN =  " "
            MOVE DEN-F02           TO  NKK-F10
     ELSE
            MOVE WK-DENNO(IX)      TO  NKK-F10
     END-IF.
*T
*    DISPLAY "NKK-F10 = " NKK-F10 UPON CONS.
*T
     IF     LINK-IN-DENKBN =  " "
            MOVE DEN-F03           TO  NKK-F11
     ELSE
            IF   SHTDENLJ-END-FLG  =  "HIT"
                 MOVE  DEN-F03     TO  NKK-F11
            END-IF
     END-IF.
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
