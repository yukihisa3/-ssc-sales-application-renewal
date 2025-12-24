# SSE0020B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE0020B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　請求合計ファイル作成　　　　　　　*
*    作成日／更新日　　　：　92/12/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　条件Ｆの締め日と一致した取引先の　*
*                        ：　請求金額を集計して，請求合計Ｆに　*
*                        ：　出力する。　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE0020B.
 AUTHOR.                S.K  SANKYO.
 DATE-WRITTEN.          92/12/03.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 伝票データ >>--*
     SELECT   HDENJNL   ASSIGN         DA-01-VI-DENJNL5
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F01   DEN-F23
                                       DEN-F04   DEN-F051
                                       DEN-F03
                        STATUS         DEN-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         JYO-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   HSEIGKF   ASSIGN         DA-01-VI-SEIGKF1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01   SEI-F05
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 伝票データ >>--*
 FD  HDENJNL            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
 01  COUNTERS.
     03  TOK-CNT        PIC  9(06).
     03  DEN-CNT        PIC  9(06).
     03  OUT-CNT        PIC  9(06).
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  TOK-ST             PIC  X(02).
 01  DEN-ST             PIC  X(02).
 01  JYO-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
 01  SIME-DATE          PIC  9(08).
 01  FILLER             REDEFINES      SIME-DATE.
     03  SIME-YY        PIC  9(04).
     03  SIME-MM        PIC  9(02).
     03  SIME-DD        PIC  9(02).
 01  SSKTLSTD-DATE      PIC  9(08).
 01  FILLER             REDEFINES      SSKTLSTD-DATE.
     03  SSKTLSTD-YY    PIC  9(04).
     03  SSKTLSTD-MM    PIC  9(02).
     03  SSKTLSTD-DD    PIC  9(02).
 01  SSKTLSTD-RET       PIC  9(01).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-TOR                        PIC  9(08).
         05  NEW-DEN                        PIC  X(09).
     03  OLD.
         05  OLD-TOR                        PIC  9(08).
         05  OLD-DEN                        PIC  X(09).
*
****************************************************************
 LINKAGE           SECTION.
****************************************************************
 01  LINK-KENSU                   PIC  9(07).
*
****************************************************************
 PROCEDURE              DIVISION         USING  LINK-KENSU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE0020B HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HTOKMS    HDENJNL   HJYOKEN   HSEIGKF.
     STOP     RUN.
*----<< 伝票データ >>--*
 HDENJNL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDENJNL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE0020B HDENJNL ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HTOKMS    HDENJNL   HJYOKEN   HSEIGKF.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE0020B HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HTOKMS    HDENJNL   HJYOKEN   HSEIGKF.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 HSEIGKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### OSKT320 HSEIGKF ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HTOKMS    HDENJNL   HJYOKEN   HSEIGKF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG   =    1.
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
     DISPLAY  "*** SSE0020B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HTOKMS.
     OPEN     I-O       HDENJNL.
     OPEN     INPUT     HJYOKEN.
     OPEN     OUTPUT    HSEIGKF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*
     MOVE     59        TO   JYO-F01.
     MOVE     SPACE     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG   =    0
              MOVE      JYO-F04   TO   SIME-DATE SSKTLSTD-DATE
              IF   SIME-DD   =    31
                   CALL "OSKTLSTD"     USING     SSKTLSTD-DATE
                                                 SSKTLSTD-RET
              END-IF
     ELSE
              MOVE      1         TO   END-FLG
     END-IF.
*
     IF       END-FLG   =    0
              PERFORM   900-TOK-READ
     END-IF.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     MOVE     SPACE          TO   SEI-REC.
     INITIALIZE                   SEI-REC.
*
     MOVE     TOK-F01        TO   DEN-F01.
     INITIALIZE         DEN-F23.
     INITIALIZE         DEN-F04.
     INITIALIZE         DEN-F051.
     INITIALIZE         DEN-F03.
     PERFORM  900-DEN-START-READ.
     MOVE     NEW            TO   OLD.
     PERFORM  210-SYUKEI
                   UNTIL     NEW       =    HIGH-VALUE.
     IF       SEI-F01        =    TOK-F01
     AND      SEI-F06   NOT  =    ZERO
              PERFORM   220-SEI-OUT
     END-IF.
*
     PERFORM  900-TOK-READ.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HTOKMS.
     CLOSE    HDENJNL.
     CLOSE    HJYOKEN.
     CLOSE    HSEIGKF.
*
     DISPLAY  "+++ ﾄｸｲｻｷ ｹﾝｽｳ =" TOK-CNT " +++" UPON CONS.
     DISPLAY  "+++ ﾃﾞﾝﾋﾟｮｳﾃﾞｰﾀ=" DEN-CNT " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     MOVE     OUT-CNT        TO   LINK-KENSU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0020B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 210-SYUKEI             SECTION.
     IF       NEW  NOT  =    OLD
              IF   SEI-F06   NOT  =    ZERO
                   PERFORM   220-SEI-OUT
              END-IF
              MOVE      SPACE     TO   SEI-REC
              INITIALIZE               SEI-REC
     END-IF.
*
     IF       DEN-F04   =    1    OR   3    OR   5
                        OR   7    OR   9
******        MULTIPLY       -1   BY   DEN-F182
              MULTIPLY       -1   BY   DEN-F181
              MULTIPLY       -1   BY   DEN-F15
     END-IF.
     IF       DEN-F051  =    41   OR   42   OR   47   OR   48
******        MULTIPLY       -1   BY   DEN-F182
              MULTIPLY       -1   BY   DEN-F181
              MULTIPLY       -1   BY   DEN-F15
     END-IF.
     MOVE     DEN-F01        TO   SEI-F01.
     MOVE     SSKTLSTD-DATE  TO   SEI-F02.
     MOVE     DEN-F07        TO   SEI-F03.
     MOVE     DEN-F113       TO   SEI-F04.
*****MOVE     DEN-F02        TO   SEI-F05. 93/03/05. ﾍﾝｺｳ H.K
     MOVE     DEN-F23        TO   SEI-F05.
***  ADD      DEN-F182       TO   SEI-F06.
     ADD      DEN-F181       TO   SEI-F06.
     MOVE     DEN-F051       TO   SEI-F07.
     MOVE     DEN-F133       TO   SEI-F08.
     ADD      DEN-F15        TO   SEI-F09.
     MOVE     DEN-F112       TO   SEI-F10.
     MOVE     DEN-F08        TO   SEI-F11.
     MOVE     DEN-F12        TO   SEI-F12.
*
     MOVE     NEW            TO   OLD.
     PERFORM  900-DEN-READ.
 210-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｾｲｷｭｳ ﾃﾞｰﾀ ｼｭﾂﾘｮｸ                           *
*--------------------------------------------------------------*
 220-SEI-OUT            SECTION.
     WRITE    SEI-REC.
     ADD      1         TO   OUT-CNT.
 220-SEI-OUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    条件ファイル　 READ                          *
*--------------------------------------------------------------*
 900-JYO-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID
              MOVE      1         TO   INV-FLG
     DISPLAY  "### SSE0020B HJYOKEN INVALID KEY=" JYO-F01 " ###"
                                       UPON CONS
     END-READ.
 900-JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    AT   END
              MOVE      1         TO   END-FLG
              GO   TO   900-TOK-READ-EXIT
     END-READ.
*
     IF       TOK-F12 (1)    NOT  =    SIME-DD
     AND      TOK-F12 (2)    NOT  =    SIME-DD
     AND      TOK-F12 (3)    NOT  =    SIME-DD
              GO   TO   900-TOK-READ
     END-IF.
     ADD      1         TO   TOK-CNT.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DEN-START-READ     SECTION.
     START    HDENJNL   KEY  >=   DEN-F01   DEN-F23
                                  DEN-F04   DEN-F051
                                  DEN-F03
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-READ
     END-IF.
 900-DEN-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     HDENJNL   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-READ.
*取引先ＣＤが異なる時
     IF       DEN-F01   NOT  =    TOK-F01
              PERFORM   900-DEN-REWRITE
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-IF.
*請求ＦＬＧ
     IF       DEN-F261  NOT  =    0    AND  1
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*行番号＝９０
     IF       DEN-F03   =    90
              MOVE      1    TO   DEN-F261
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*売上データ
     IF       DEN-F277  NOT  =    9
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*画面指定月末日
     IF       DEN-F112  >    SSKTLSTD-DATE
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*行番号＝８０
     IF       DEN-F03   =    80
              MOVE      1    TO   DEN-F261
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*請求ＦＬＧセット
     MOVE     1         TO   DEN-F261.
     PERFORM  900-DEN-REWRITE.
*\\ 93/08/05  START \\
*
     IF       DEN-F133  =    9
              GO   TO   900-DEN-READ
     END-IF.
*\\ 93/08/05  END   \\
     ADD      1         TO   DEN-CNT.
*
     MOVE     DEN-F01        TO   NEW-TOR.
     MOVE     DEN-F23        TO   NEW-DEN.
 900-DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 REWRITE                       *
*--------------------------------------------------------------*
 900-DEN-REWRITE        SECTION.
     REWRITE  DEN-REC.
 900-DEN-REWRITE-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
