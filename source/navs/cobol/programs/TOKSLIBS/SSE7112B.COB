**************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　*
*    モジュール名　　　　：　Ｊ本田仕入実績請求データ作成    *
*    作成日／更新日　　　：　05/08/11　　　　　　　　　　　　*
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　*
*    処理概要　　　　　　：　条件ﾌｧｲﾙと仕入実績集計ファイルよ*
*                        ：　仕入実績請求データを作成する　　*
*    作成日／更新日　　　：　08/08/28                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　*
*    処理概要　　　　　　：　内部統制対応　　　　　　　　　　*
*    作成日／更新日　　　：　17/04/03                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　*
*    処理概要　　　　　　：　店舗５桁対応　　　　　　　　　　*
**************************************************************
**************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE7112B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/08/11.
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
*----<< 仕入実績集計データ >>--*
     SELECT   JDSYUKF   ASSIGN         DA-01-VI-JDSYUKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SYU-F04   SYU-F02
                                       SYU-F03
                        STATUS         SYU-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         JYO-ST.
*----<< 仕入実績請求データ >>--*
     SELECT   JHJSEKF   ASSIGN         DA-01-VI-JHJSEKL1
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
 FD  JDSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     JDSYUKF   OF        XFDLIB
              JOINING   SYU       PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL     RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< 仕入実績集計データ >>--*
 FD  JHJSEKF            LABEL     RECORD   IS   STANDARD.
     COPY     JHJSEKF   OF        XFDLIB
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
 01  SYU-ST             PIC  X(02).
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
 LINKAGE                SECTION.
 01  PARA-OUT-CNT                           PIC  9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 JDSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JDSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE7112B JDSYUKF ERROR " SYU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSE7112B HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 仕入実績請求Ｆ >>--*
 JHJSEKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JHJSEKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### OSKT320 JHJSEKF ERROR " SEI-ST " "
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
     DISPLAY  "*** SSE7112B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       JDSYUKF.
     OPEN     INPUT     HJYOKEN.
     OPEN     I-O       JHJSEKF.
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
*    伝票番号
     INITIALIZE              SYU-F02.
*    伝票区分
     INITIALIZE              SYU-F06.
*    店舗コード
     INITIALIZE              SYU-F04.
*    行番号
     INITIALIZE              SYU-F03.

     MOVE      ZERO          TO   WK-KINGAKU
                                  WK-SURYO.

     PERFORM  900-SYU-START-READ.
     IF       END-FLG  =  1
              GO             TO   200-MAIN-RTN-EXIT
     END-IF.
     MOVE     NEW            TO   OLD.
     PERFORM  210-SYUKEI
                   UNTIL     END-FLG   =    1.
*    請求金額が０以上
     IF       SEI-F10   NOT  =    ZERO
              PERFORM   220-SEI-OUT
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    JDSYUKF.
     CLOSE    HJYOKEN.
     CLOSE    JHJSEKF.
*
     DISPLAY  "+++ ﾃﾞﾝﾋﾟｮｳﾃﾞｰﾀ=" DEN-CNT " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
*内部統制対応 2008/08/28
     MOVE     OUT-CNT        TO  PARA-OUT-CNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE7112B END *** "
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
              IF   SEI-F10   NOT  =    ZERO
                   PERFORM   220-SEI-OUT
              END-IF
              MOVE      SPACE     TO   SEI-REC
              INITIALIZE               SEI-REC
              MOVE      ZERO      TO   IND      WK-GOUKEI
                                       WK-SURYO WK-KINGAKU
     END-IF.
     IF       NEW-GYO   NOT   =   OLD-GYO
              ADD       1         TO   IND
              MOVE      ZERO      TO   WK-KINGAKU
                                       WK-SURYO
     END-IF.
*             伝票区分
     IF       SYU-F06   =    41   OR   42
              MULTIPLY       -1   BY   SYU-F10
              MULTIPLY       -1   BY   SYU-F08
     END-IF.
*
*    締日
     MOVE     SSKTLSTD-DATE  TO   SEI-F01.
*    店舗コード
     MOVE     SYU-F04        TO   SEI-F02.
*    伝票番号
     MOVE     SYU-F02        TO   SEI-F03.
*    取引日付
     MOVE     SYU-F01        TO   SEI-F04.
*    部門コード
     MOVE     SYU-F11        TO   SEI-F05.
*    項目コード
     MOVE     SYU-F12        TO   SEI-F06.
*    伝票区分
     MOVE     SYU-F06        TO   SEI-F07.
*    区分コード
     MOVE     ZERO           TO   SEI-F08.
     MOVE     SYU-F13        TO   SEI-F08.
*    数量
     COMPUTE  WK-SURYO       =    WK-SURYO  +  SYU-F08.
     MOVE     WK-SURYO       TO   SEI-F091(IND).
*    単価
     MOVE     SYU-F09        TO   SEI-F092(IND).
*    金額
     COMPUTE  WK-KINGAKU     =    SEI-F091(IND) * SEI-F092(IND).
     MOVE     WK-KINGAKU     TO   SEI-F093(IND).
*
*    合計金額
     COMPUTE  WK-GOUKEI      =    WK-GOUKEI + WK-KINGAKU
     MOVE     WK-GOUKEI      TO   SEI-F10
     MOVE     NEW            TO   OLD.
     PERFORM  900-SYU-READ.
 210-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｾｲｷｭｳ ﾃﾞｰﾀ ｼｭﾂﾘｮｸ                           *
*--------------------------------------------------------------*
 220-SEI-OUT            SECTION.
*     MOVE     SSKTLSTD-DATE  TO   SEI-F01.
*     MOVE     DEN-F07        TO   SEI-F02.
*     MOVE     DEN-F23        TO   SEI-F03.
     READ     JHJSEKF        INVALID
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
     DISPLAY  "### SSE7112B HJYOKEN INVALID KEY=" JYO-F01 " ###"
                                       UPON CONS
     END-READ.
 900-JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    仕入実績集計ファイル　 START READ            *
*--------------------------------------------------------------*
 900-SYU-START-READ     SECTION.
     START    JDSYUKF   KEY  >=   SYU-F04   SYU-F02
                                  SYU-F03
              INVALID   KEY
                        MOVE      1         TO   END-FLG
                        GO   TO   900-SYU-START-READ-EXIT
     END-START.
     IF       END-FLG  NOT =  1
              PERFORM   900-SYU-READ
     END-IF.
 900-SYU-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    仕入実績集計ファイル　 READ                  *
*--------------------------------------------------------------*
 900-SYU-READ           SECTION.
     READ     JDSYUKF   AT   END
              MOVE      1              TO   END-FLG
              GO   TO   900-SYU-READ-EXIT
     END-READ.
 902-SYU.
*請求ＦＬＧ
     IF       SYU-F99   NOT  =    SPACE  AND  1
              GO   TO   900-SYU-READ
     END-IF.
*締日以上の場合は読み飛ばし
 903-SYU.
     IF       SYU-F01  >  SIME-DATE
              GO                       TO   900-SYU-READ
     END-IF.
*請求ＦＬＧセット
     MOVE     9         TO   SYU-F99.
     PERFORM  900-SYU-REWRITE.
     ADD      1         TO   DEN-CNT.
*
     MOVE     SSKTLSTD-DATE  TO   NEW-SIM.
     MOVE     SYU-F04        TO   NEW-TEN
     MOVE     SYU-F02        TO   NEW-DEN.
     MOVE     SYU-F03        TO   NEW-GYO.
 900-SYU-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    仕入実績集計ファイル　 REWRITE               *
*--------------------------------------------------------------*
 900-SYU-REWRITE        SECTION.
     REWRITE  SYU-REC.
 900-SYU-REWRITE-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
