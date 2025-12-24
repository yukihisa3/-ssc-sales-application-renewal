****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　片倉工業支払照合　　　　　　　　　*
*    モジュール名　　　　：　照合ファイル作成　　　　　　　　　*
*    作成日／更新日　　　：　95/11/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払明細ファイルと請求明細ファイル*
*                        ：　の金額を照合する。　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0090B.
 AUTHOR.                N.T.
 DATE-WRITTEN.          95/11/15.
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
*----<< 支払明細ファイル >>--*
     SELECT   HSHIGKF   ASSIGN         DA-01-S-HSHIGKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHI-ST.
*----<< 支払明細ファイル >>--*
     SELECT   HSEIGKF   ASSIGN         DA-01-VI-SEIGKF21
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01  SEI-F05
                        STATUS         SEI-ST.
*----<< 照合ファイル >>--*
     SELECT   SYOGOUF   ASSIGN         DA-01-S-SYOGOUF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SYO-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  HSHIGKF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 20   RECORDS.
     COPY     SITGKFA   OF        XFDLIB
              JOINING   SHI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 照合ファイル >>--*
 FD  SYOGOUF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 20   RECORDS.
     COPY     SITGKFA   OF        XFDLIB
              JOINING   SYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHI-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  SYO-ST             PIC  X(02).
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
*----<< READ KEY >>--*
 01  SHI-KEY.
     03  SHI-DENNO      PIC  X(09).
 01  SEI-KEY.
     03  SEI-DENNO      PIC  X(09).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払明細ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0090B HSHIGKF ERROR " SHI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HSHIGKF   HSEIGKF   SYOGOUF.
     STOP     RUN.
*----<< 請求明細ファイル >>--*
 SEI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0090B HSEIGKF ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HSHIGKF   HSEIGKF   SYOGOUF.
     STOP     RUN.
*----<< 照合ファイル >>--*
 SYO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYOGOUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0090B SYOGOUF ERROR " SYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HSHIGKF   HSEIGKF   SYOGOUF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     SHI-KEY  =  HIGH-VALUE
                                  AND  SEI-KEY  =  HIGH-VALUE.
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
     DISPLAY  "*** SSI0090B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HSHIGKF.
     OPEN     INPUT     HSEIGKF.
     OPEN     OUTPUT    SYOGOUF.
*
     MOVE     LOW-VALUE      TO   SHI-KEY   SEI-KEY.
     PERFORM  900-SHI-READ.
     PERFORM  900-SEI-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     EVALUATE  TRUE
         WHEN      SHI-KEY   <    SEI-KEY
               PERFORM  210-HENSYU-RTN
               PERFORM  900-SHI-READ
         WHEN      SHI-KEY   =    SEI-KEY
               PERFORM  220-HENSYU-RTN
               PERFORM  900-SHI-READ
               PERFORM  900-SEI-READ
         WHEN      SHI-KEY   >    SEI-KEY
               PERFORM  230-HENSYU-RTN
               PERFORM  900-SEI-READ
     END-EVALUATE.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HSHIGKF.
     CLOSE    HSEIGKF.
     CLOSE    SYOGOUF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0090B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     照合ファイル出力（請求データ無）             *
*--------------------------------------------------------------*
 210-HENSYU-RTN         SECTION.
     MOVE     SHI-REC             TO   SYO-REC.
     MOVE     "ｾｲｷｭｳﾃﾞｰﾀﾅｼ"       TO   SYO-F14.
     MOVE     "1"                 TO   SYO-F24.
     WRITE    SYO-REC.
 210-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     照合ファイル出力（金額チェック）             *
*--------------------------------------------------------------*
 220-HENSYU-RTN         SECTION.
     MOVE     SHI-REC             TO   SYO-REC.
     IF       SHI-F22     =   SEI-F06
              MOVE     SPACE               TO   SYO-F14
                                                SYO-F24
     ELSE
              MOVE     "ｷﾝｶﾞｸﾁｶﾞｲ"         TO   SYO-F14
              MOVE     "1"                 TO   SYO-F24
              MOVE     SHI-F22             TO   SYO-F26
     END-IF.
     WRITE    SYO-REC.
 220-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     照合ファイル出力（支払データ無）             *
*--------------------------------------------------------------*
 230-HENSYU-RTN         SECTION.
     MOVE     SPACE               TO   SYO-REC.
     INITIALIZE                        SYO-REC.
     MOVE     SEI-F02             TO   SYO-F11.
     MOVE     "ｼﾊﾗｲﾃﾞｰﾀﾅｼ"        TO   SYO-F14.
     MOVE     SEI-F03             TO   SYO-F19.
     MOVE     SEI-F04             TO   SYO-F20.
     MOVE     SEI-F05             TO   SYO-F21.
     MOVE     "1"                 TO   SYO-F24.
     MOVE     SEI-F01             TO   SYO-F32.
     WRITE    SYO-REC.
 230-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払明細ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SHI-READ           SECTION.
     READ     HSHIGKF
         AT END
              MOVE      HIGH-VALUE     TO   SHI-KEY
         NOT AT END
              MOVE      SHI-F21        TO   SHI-DENNO
     END-READ.
 900-SHI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求明細ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     READ     HSEIGKF
         AT END
              MOVE      HIGH-VALUE     TO   SEI-KEY
         NOT AT END
              MOVE      SEI-F05        TO   SEI-DENNO
     END-READ.
 900-SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
