# SSE7101B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSE7101B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　ジョイフル本田請求ファイル作成　　*
*    作成日／更新日　　　：　05/08/10                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　条件ファイルと売上伝票ファイルより*
*                        ：　請求ファイルを作成する　　　　　　*
*                        ：　          　　　　　　　　　　　
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE7101B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/08/10.
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
*----<< 伝票データ >>--*
     SELECT   HDENJNL   ASSIGN         DA-01-VI-SHTDENL5
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
*----<< 請求Ｆ >>--*
     SELECT   JDSEIKF   ASSIGN         DA-01-VI-JDSEIKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01   SEI-F02
                                       SEI-F03
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  HDENJNL            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL     RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< 請求Ｆ >>--*
 FD  JDSEIKF            LABEL     RECORD   IS   STANDARD.
     COPY     JDSEIKF   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
     03  WRT-FLG        PIC  9(01).
 01  COUNTERS.
     03  DEN-CNT        PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
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
*金額計算用ワーク
 01  WK-KINGAKU         PIC  S9(10).
*合計金額計算用ワーク
 01  WK-GOUKEI          PIC  S9(10).
*数量ワーク
 01  WK-SURYO           PIC  S9(10).
 01  IND                PIC  99.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-SIM                        PIC  9(08).
         05  NEW-TEN                        PIC  9(03).
         05  NEW-DEN                        PIC  9(07).
         05  NEW-GYO                        PIC  9(02).
     03  OLD.
         05  OLD-SIM                        PIC  9(08).
         05  OLD-TEN                        PIC  9(03).
         05  OLD-DEN                        PIC  9(07).
         05  OLD-GYO                        PIC  9(02).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 HDENJNL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDENJNL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE7101B HDENJNL ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE7101B HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求Ｆ >>--*
 JDSEIKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JDSEIKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### OSKT320 JDSEIKF ERROR " SEI-ST " "
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
     DISPLAY  "*** SSE7101B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       HDENJNL.
     OPEN     INPUT     HJYOKEN.
     OPEN     I-O       JDSEIKF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*締日取得
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
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     MOVE     SPACE          TO   SEI-REC.
     INITIALIZE                   SEI-REC.
     MOVE     1              TO   IND.
*    指定伝票番号
     INITIALIZE         DEN-F23.
*    伝票区分
     INITIALIZE         DEN-F132.
*    伝区コード
     INITIALIZE         DEN-F051.
*    行番号
     INITIALIZE         DEN-F03.
     MOVE      ZERO      TO   WK-KINGAKU
                              WK-SURYO.

     MOVE     2243           TO   DEN-F01.
     PERFORM  900-DEN-START-READ.
     IF       END-FLG  =  1
              GO             TO   200-MAIN-RTN-EXIT
     END-IF.
     MOVE     NEW            TO   OLD.
     PERFORM  210-SYUKEI
                   UNTIL     END-FLG   =    1.
*請求金額が０以上
     IF       SEI-F06   NOT  =    ZERO
              PERFORM   220-SEI-OUT
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HDENJNL.
     CLOSE    HJYOKEN.
     CLOSE    JDSEIKF.
*
     DISPLAY  "+++ ﾃﾞﾝﾋﾟｮｳﾃﾞｰﾀ=" DEN-CNT " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE7101B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 210-SYUKEI             SECTION.
     IF       NEW-DEN  NOT   =    OLD-DEN
     OR       NEW-SIM  NOT   =    OLD-SIM
     OR       NEW-TEN  NOT   =    OLD-TEN
*             請求金額
              IF   SEI-F06   NOT  =    ZERO
                   PERFORM   220-SEI-OUT
              END-IF
              MOVE      SPACE     TO   SEI-REC
              INITIALIZE               SEI-REC
              MOVE      ZERO      TO   IND      WK-GOUKEI
                                       WK-SURYO WK-KINGAKU
     END-IF.
     IF       NEW-GYO  NOT   =    OLD-GYO
              ADD       1         TO   IND
              MOVE      ZERO      TO   WK-KINGAKU
                                       WK-SURYO
     END-IF.
*             伝区コード
     IF       DEN-F051       =    41   OR   42
              MULTIPLY       -1   BY   DEN-F181
              MULTIPLY       -1   BY   DEN-F15
     END-IF.
*
*    締日
     MOVE     SSKTLSTD-DATE  TO   SEI-F01.
*    店舗コード
     MOVE     DEN-F07        TO   SEI-F02.
*    伝票番号
     MOVE     DEN-F23        TO   SEI-F03.
*    取引日付
     MOVE     DEN-F112       TO   SEI-F04.
*    部門コード
     MOVE     DEN-F12(1:2)   TO   SEI-F05.
*    項目コード
     MOVE     SPACE          TO   SEI-F06.
*    伝票区分
*****MOVE     DEN-F051       TO   SEI-F07.
     EVALUATE DEN-F051
         WHEN "40"
              MOVE  "31"     TO   SEI-F07
         WHEN "41"
              MOVE  "21"     TO   SEI-F07
         WHEN "42"
              MOVE  "11"     TO   SEI-F07
     END-EVALUATE.
*    区分コード
     MOVE     ZERO           TO   SEI-F08.
*    数量
     COMPUTE  WK-SURYO       =    WK-SURYO  +  DEN-F15.
     MOVE     WK-SURYO       TO   SEI-F091(IND).
*****DISPLAY  "数量： " DEN-F15    UPON   CONS.
*    単価
     MOVE     DEN-F172       TO   SEI-F092(IND).
*    金額
     COMPUTE  WK-KINGAKU     =    SEI-F091(IND) * SEI-F092(IND).
     MOVE     WK-KINGAKU     TO   SEI-F093(IND).
*
*    合計金額
     COMPUTE  WK-GOUKEI      =    WK-GOUKEI + WK-KINGAKU
     MOVE     WK-GOUKEI      TO   SEI-F10
     MOVE     NEW            TO   OLD.
     PERFORM  900-DEN-READ.
 210-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｾｲｷｭｳ ﾃﾞｰﾀ ｼｭﾂﾘｮｸ                           *
*--------------------------------------------------------------*
 220-SEI-OUT            SECTION.
*     MOVE     SSKTLSTD-DATE  TO   SEI-F01.
*     MOVE     DEN-F07        TO   SEI-F02.
*     MOVE     DEN-F23        TO   SEI-F03.
     READ     JDSEIKF        INVALID
              MOVE      0    TO   WRT-FLG
              NOT            INVALID
              MOVE      1    TO   WRT-FLG
     END-READ.
     IF       WRT-FLG   = 0
              WRITE       SEI-REC
     ELSE
              REWRITE     SEI-REC
     END-IF.
*     DISPLAY  "***伝票番号1＝" DEN-F23 UPON CONS.
*     DISPLAY  "***伝票番号2＝" SEI-F03 UPON CONS.
     ADD      1         TO   OUT-CNT.
     MOVE     0         TO   WRT-FLG.
 220-SEI-OUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    条件ファイル　 READ                          *
*--------------------------------------------------------------*
 900-JYO-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID
              MOVE      1         TO   INV-FLG
     DISPLAY  "### SSE7101B HJYOKEN INVALID KEY=" JYO-F01 " ###"
                                       UPON CONS
     END-READ.
 900-JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DEN-START-READ     SECTION.
     START    HDENJNL   KEY  >=   DEN-F01   DEN-F23
                                  DEN-F04   DEN-F051
                                  DEN-F03
              INVALID   KEY
                        MOVE      1         TO   END-FLG
                        GO   TO   900-DEN-START-READ-EXIT
     END-START.
     IF       END-FLG  NOT =  1
              PERFORM   900-DEN-READ
     END-IF.
 900-DEN-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     HDENJNL   AT   END
              MOVE      1              TO   END-FLG
              GO   TO   900-DEN-READ-EXIT
     END-READ.
 902-DEN.
*ジョイフル本田以上になった場合
     IF       DEN-F01   > 2243
              MOVE      1              TO   END-FLG
              GO   TO   900-DEN-READ-EXIT
     END-IF.
*請求ＦＬＧ
     IF       DEN-F261  NOT  =    0    AND  1
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
 903-DEN.
*行番号＝９０
     IF       DEN-F03   =    90
              MOVE      1    TO   DEN-F261
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
 904-DEN.
*行番号＝８０
     IF       DEN-F03   =    80
              MOVE      1    TO   DEN-F261
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
 905-DEN.
*売上データ
     IF       DEN-F277  NOT  =    9
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
 906-DEN.
*画面指定月末日
     IF       DEN-F112  >    SSKTLSTD-DATE
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*請求ＦＬＧセット
     MOVE     1         TO   DEN-F261.
     PERFORM  900-DEN-REWRITE.
*\\ 93/08/05  START \\
*
 907-DEN.
     IF       DEN-F133  =    9
              GO   TO   900-DEN-READ
     END-IF.
*\\ 93/08/05  END   \\
     ADD      1         TO   DEN-CNT.
*
     MOVE     SSKTLSTD-DATE  TO   NEW-SIM.
     MOVE     DEN-F07        TO   NEW-TEN
     MOVE     DEN-F23        TO   NEW-DEN.
     MOVE     DEN-F03        TO   NEW-GYO.
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
