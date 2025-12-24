****************************************************************
*                                                              *
*                 ì`ï[_ÇbÇmÇsÅ@Å@Å@Å@Å@                       *
*                            S S K T 0 5 5                     *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY0604T.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONSL
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*ì`ï[ÉfÅ[É^ÇeÇQ
     SELECT      HDENJNL2    ASSIGN    TO        DA-01-S-KYOTUX
                             FILE      STATUS    DEN2-ST.
*ì`ï[ÉfÅ[É^Çe
     SELECT      HDENJNL     ASSIGN    TO        DA-01-S-HDENJNL
                             FILE      STATUS    DEN-ST.
*éÊà¯êÊÉ}ÉXÉ^
     SELECT      HTOKMS      ASSIGN    TO        DA-01-VI-TOKMS2
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TOK-F01
                             FILE      STATUS    TOK-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*ì`ï[ÉfÅ[É^Çe
 FD  HDENJNL2
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    15        RECORDS.
     COPY        HDENJNL2    OF        XFDLIB
     JOINING     DEN2        AS        PREFIX.
*ì`ï[ÉfÅ[É^Çe
 FD  HDENJNL
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    15        RECORDS.
     COPY        SHTDENF     OF        XFDLIB
     JOINING     DEN         AS        PREFIX.
*éÊà¯êÊÉ}ÉXÉ^
 FD  HTOKMS
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS     8        RECORDS.
     COPY        HTOKMS      OF        XFDLIB
     JOINING     TOK         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  DEN2-ST             PIC  X(02)  VALUE  SPACE.
*****03  DEN2-ST1            PIC  X(04)  VALUE  SPACE.
     03  DEN-ST              PIC  X(02)  VALUE  SPACE.
*****03  DEN-ST1             PIC  X(04)  VALUE  SPACE.
     03  TOK-ST              PIC  X(02)  VALUE  SPACE.
*****03  TOK-ST1             PIC  X(04)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  INV-SW              PIC  9(01)  VALUE  ZERO.
     03  WK-DENCNT           PIC  9(09)  VALUE  ZERO.
     03  IX                  PIC  9(01)  VALUE  ZERO.
     03  WK-TOKCD            PIC  9(08)  VALUE  ZERO.
     03  WK-TENCD            PIC  9(05)  VALUE  ZERO.
     03  WK-DENNO            PIC  9(09)  VALUE  ZERO.
*\\\ 93.06.04 START \\\
     03  WK-SYUKA            PIC  9(08)  VALUE  ZERO.
     03  WK-BASYO            PIC  X(02)  VALUE  SPACE.
*\\\ 93.06.04 END   \\\
 01  DENNO-S                 PIC  9(09)  VALUE  999999999.
 01  DENNO-L                 PIC  9(09)  VALUE  ZERO.
*
*---<< ªÃﬁŸ∞¡› LINK AREA >>-*
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-KBN          PIC  9(01).
         05  LI-KETA         PIC  9(01).
         05  LI-START        PIC  9(09).
         05  LI-END          PIC  9(09).
         05  LI-DENNO        PIC  9(09).
     03  LINK-OUT.
         05  LO-ERR          PIC  9(01).
         05  LO-NEXT         PIC  9(09).
*
 01  FILE-ERR.
     03  DEN2-ERR            PIC  N(10)  VALUE
                   NC"ì`ï[ÉfÅ[É^ÇeÇQàŸèÌ".
     03  DEN-ERR             PIC  N(10)  VALUE
                   NC"ì`ï[ÉfÅ[É^ÇeàŸèÌ".
     03  TOK-ERR             PIC  N(10)  VALUE
                   NC"éÊà¯êÊÉ}ÉXÉ^àŸèÌ".
****************************************************************
 PROCEDURE                   DIVISION.
****************************************************************
 DECLARATIVES.
 DEN2-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HDENJNL2.
     DISPLAY     DEN2-ERR    UPON      STAT.
     DISPLAY     DEN2-ST     UPON      STAT.
*****DISPLAY     DEN2-ST1    UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HDENJNL.
     DISPLAY     DEN-ERR     UPON      STAT.
     DISPLAY     DEN-ST      UPON      STAT.
*****DISPLAY     DEN-ST1     UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR     UPON      STAT.
     DISPLAY     TOK-ST      UPON      STAT.
*****DISPLAY     TOK-ST1     UPON      STAT.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      STAT.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = 9.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     OPEN        INPUT       HDENJNL2
                 EXTEND      HDENJNL
                 I-O         HTOKMS.
*ó îÃÇqÇdÇbèâä˙âª
     MOVE        SPACE       TO   DEN-REC.
     INITIALIZE  DEN-REC.
 INIT-010.
*ì`ï[ÉfÅ[É^ÇQèâä˙ì«Ç›
     PERFORM  DEN2-RD-SEC.
     IF    END-FLG  =  0
       MOVE     DEN2-F01       TO   WK-TOKCD
       MOVE     DEN2-F02       TO   WK-DENNO
       MOVE     DEN2-F07       TO   WK-TENCD
*\\\ 93.06.04 START \\\
       MOVE     DEN2-F112      TO   WK-SYUKA
       MOVE     DEN2-F08       TO   WK-BASYO
*\\\ 93.06.04 END   \\\
*éÊà¯êÊÉ}ÉXÉ^ÇqÇdÇ`Çc
       PERFORM  TOK-RD-SEC
       IF   INV-SW  =  1
              GO        TO        INIT-010
       END-IF
     END-IF.
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
*éÊà¯êÊÉRÅ[ÉhÇ™ÉuÉåÉCÉN
     IF  DEN2-F01 NOT = WK-TOKCD
         PERFORM   TOK-WT-SEC
         MOVE      DEN2-F01       TO   WK-TOKCD
         MOVE      DEN2-F02       TO   WK-DENNO
         MOVE      DEN2-F07       TO   WK-TENCD
*\\\ 93.06.04 START \\\
         MOVE      DEN2-F112      TO   WK-SYUKA
         MOVE      DEN2-F08       TO   WK-BASYO
*\\\ 93.06.04 END   \\\
         PERFORM   TOK-RD-SEC
         IF   (INV-SW  =  1)
           GO             TO        DEN2-READ
         END-IF
     END-IF.
*éwíËì`ï[_ñîÇÕìXï‹ÉRÅ[ÉhÇ™ÉuÉåÉCÉN
     IF  DEN2-F02 NOT = WK-DENNO
     OR  DEN2-F07 NOT = WK-TENCD
*
*\\\ 93.06.04 START \\\
     OR  DEN2-F112 NOT = WK-SYUKA
     OR  DEN2-F08  NOT = WK-BASYO
*\\\ 93.06.04 END   \\\
*
         PERFORM   DEN-GET-SEC
         MOVE      0              TO   IX
         MOVE      DEN2-F02       TO   WK-DENNO
         MOVE      DEN2-F07       TO   WK-TENCD
*\\\  93.06.04 START \\\
         MOVE      DEN2-F112      TO   WK-SYUKA
         MOVE      DEN2-F08       TO   WK-BASYO
*\\\  93.06.04 END   \\\
     END-IF.
*ìEóvÅiçs_ÅÅÇWÇOÅjÇÕÇªÇÃÇ‹Ç‹ÉâÉCÉg
** 98/07/16 TAKAHASHI Õ›∫≥ START **
** èåèÇ…è§ïiÉRÅ[ÉhÇ™îıçlàµÇ¢ÇÃéûÇí«â¡Ç∑ÇÈ
** ÉÅÉÇ_Ç©ÇÁçs_ÇçÏê¨ÇµÇƒÇ¢ÇÈà◊ÅCí èÌè§ïiÇ≈Ç†Ç¡ÇƒÇ‡îıçlóìÇ∆
** ÇµÇƒàµÇ¡ÇƒÇµÇ‹Ç§à◊
     IF  DEN2-F03  =      80
     AND DEN2-F1411 =    "99      "
       INITIALIZE              DEN-REC
       MOVE     DEN2-REC       TO        DEN-REC
       MOVE     WK-DENCNT      TO        DEN-F02
       MOVE     WK-DENCNT      TO        DEN-F23
       IF       DEN-F02        <         DENNO-S
                MOVE    DEN-F02   TO     DENNO-S
       END-IF
       IF       DEN-F02        >         DENNO-L
                MOVE    DEN-F02   TO     DENNO-L
       END-IF
*
       WRITE    DEN-REC
       GO       TO        DEN2-READ
     END-IF.
** 98/07/16 TAKAHASHI Õ›∫≥ END   **
*çs_ÇÕÇUÇ‹Ç≈
     ADD   1       TO     IX.
     IF    IX      >   6
*********ADD       1              TO   WK-DENCNT
         PERFORM   DEN-GET-SEC
         MOVE      1              TO   IX
     END-IF.
*ì`ï[ÉfÅ[É^çÏê¨
     INITIALIZE              DEN-REC
     MOVE     DEN2-REC       TO        DEN-REC.
     MOVE     WK-DENCNT      TO        DEN-F02.
     MOVE     WK-DENCNT      TO        DEN-F23.
     MOVE     ZERO           TO        DEN-F113.
     MOVE     IX             TO        DEN-F03.
       IF       DEN-F02        <         DENNO-S
                MOVE    DEN-F02   TO     DENNO-S
       END-IF.
       IF       DEN-F02        >         DENNO-L
                MOVE    DEN-F02   TO     DENNO-L
       END-IF.
*
     WRITE    DEN-REC.
*ì`ï[ÉfÅ[É^ÇQÉäÅ[Éh
 DEN2-READ.
     PERFORM  DEN2-RD-SEC.
     IF    END-FLG  =  9
         PERFORM   TOK-WT-SEC
     END-IF.
 MAIN-EXIT.
     EXIT.
****************************************************************
*               ì` ï[ Éf Å[ É^ ÇQ Çq Çd Ç` Çc
****************************************************************
 DEN2-RD-SEC                 SECTION.
     READ     HDENJNL2  AT   END
              MOVE      9         TO   END-FLG
              GO        TO        DEN2-RD-EXIT.
 DEN2-RD-EXIT.
     EXIT.
****************************************************************
*               éÊ à¯ êÊ É} ÉX É^ Çq Çd Ç` Çc
****************************************************************
 TOK-RD-SEC                  SECTION.
     MOVE     ZERO           TO   INV-SW
     MOVE     WK-TOKCD       TO   TOK-F01.
     READ     HTOKMS
       INVALID
              MOVE      1         TO   INV-SW
       NOT INVALID
              MOVE      TOK-F54   TO   WK-DENCNT
              PERFORM   DEN-GET-SEC
              MOVE      ZERO      TO   IX
     END-READ.
 TOK-RD-EXIT.
     EXIT.
****************************************************************
*              éÊà¯êÊÉ}ÉXÉ^ ÇqÇdÇvÇqÇhÇsÇd
****************************************************************
 TOK-WT-SEC                  SECTION.
     MOVE     WK-DENCNT      TO   TOK-F54.
     REWRITE  TOK-REC.
 TOK-WT-EXIT.
     EXIT.
******************************************************************
*                  ì`ï[î‘çÜéÊìæ
******************************************************************
 DEN-GET-SEC                 SECTION.
     INITIALIZE              LINK-AREA.
     MOVE     TOK-F93        TO   LI-KBN.
     MOVE     TOK-F92        TO   LI-KETA.
     MOVE     TOK-F57        TO   LI-START.
     MOVE     TOK-F58        TO   LI-END.
     MOVE     WK-DENCNT      TO   LI-DENNO.
 DEN-010.
     CALL     "OSKTCDCK"     USING     LINK-AREA.
 DEN-020.
     IF       LO-ERR  =  0
              MOVE      LO-NEXT   TO   WK-DENCNT
              MOVE      LO-NEXT   TO   TOK-F54
     ELSE
              DISPLAY   NC"ì`ï[_çÃî‘ÉGÉâÅ["  UPON STAT
              DISPLAY   WK-DENCNT             UPON STAT
              DISPLAY   TOK-ST      UPON      STAT
**************DISPLAY   TOK-ST1     UPON      STAT
**************MOVE      255         TO        PROGRAM-STATUS
              ACCEPT    IN-DATA     FROM      STAT
              STOP  RUN
     END-IF.
 DEN-GET-EXIT.
     EXIT.
****************************************************************
*    èIóπ
****************************************************************
 END-SEC                   SECTION.
*
     DISPLAY  "ÅñÅñó îÃìXÅ@ÉfÅ[É^ïœä∑ÅñÅñÅñ" UPON CONSL.
     DISPLAY  "ÅñÅñäJénì`ï[_Å@ÅÅÅ@" DENNO-S UPON CONSL.
     DISPLAY  "ÅñÅñèIóπì`ï[_Å@ÅÅÅ@" DENNO-L UPON CONSL.
*
     CLOSE    HDENJNL2   HDENJNL   HTOKMS.
 END-EXIT.
     EXIT.
